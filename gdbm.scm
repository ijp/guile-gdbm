(define-module (gdbm)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (gdbm-db?
            ;; flags
            GDBM_READER
            GDBM_WRITER
            GDBM_WRCREAT
            GDBM_NEWDB
            GDBM_SYNC
            GDBM_NOLOCK
            GDBM_NOMMAP
            ;; procedures
            gdbm-open
            gdbm-close))

;;; utilities

(define free
  (let ((this (dynamic-link)))
    (pointer->procedure void (dynamic-func "free" this) '(*))))

(define datum (list '* int))

(define-syntax-rule (define-foreign name ret string-name args)
  (define name
    (pointer->procedure ret (dynamic-func string-name libgdbm) args)))

;;; low-level libgdbm access

(define libgdbm (dynamic-link "libgdbm"))

(define-foreign %gdbm-open '* "gdbm_open" (list '* int int int '*))

(define-foreign %gdbm-close void "gdbm_close" '(*))

(define-foreign %gdbm-store int "gdbm_store" (list '* datum datum int))

(define-foreign %gdbm-fetch datum "gdbm_fetch" (list '* datum))

(define-foreign %gdbm-delete int "gdbm_delete" (list '* datum))

(define-foreign %gdbm-first-key datum "gdbm_firstkey" '(*))

(define-foreign %gdbm-next-key datum "gdbm_nextkey" (list '* datum))

(define-foreign %gdbm-reorganize int "gdbm_reorganize" '(*))

(define-foreign %gdbm-sync void "gdbm_sync" '(*))

(define-foreign %gdbm-exists int "gdbm_exists" '(* *))

(define-foreign %gdbm-strerror '* "gdbm_strerror" (list int))

(define-foreign %gdbm-setopt int "gdbm_setopt" `(* ,int * ,int))

(define-foreign %gdbm-fdesc int "gdbm_fdesc" '(*))

;;; gdbm types

(define-wrapped-pointer-type gdbm-db
  gdbm-db?
  wrap-db
  unwrap-db
  (lambda (db port)
    ;; just a simple way to distinguish different dbs for now
    (format port "#<gdbm-db ~a>" (pointer-address (unwrap-db db)))))

(define maximum-int (- (expt 2 (* 8 (sizeof int))) 1))

(define (string->db-datum string)
  (let ((bv (string->utf8 string)))
    (when (> (bytevector-length bv) maximum-int)
      (error "string is too large for db"))
    (make-c-struct datum
                   (list (bytevector->pointer bv)
                         (bytevector-length bv)))))

(define (db-datum->string db-datum)
  (let* ((struct (parse-c-struct db-datum datum))
         (bv-pointer (car struct))
         (bv-length (cadr struct)))
    (if (null-pointer? bv-pointer)
        #f
        (let* ((bv (pointer->bytevector bv-pointer bv-length))
               (str (utf8->string bv)))
          (free bv-pointer)
          str))))

;;; errors

(define %errno (dynamic-pointer "gdbm_errno" libgdbm))

(define (gdbm-errno)
  (pointer-address (dereference-pointer %errno)))

(define (gdbm-error)
  (error (pointer->string (%gdbm-strerror (gdbm-errno)))))

;;; open flags

;; currently copied from gdbm.h, should really be generated or something
(define GDBM_READER 0)
(define GDBM_WRITER 1)
(define GDBM_WRCREAT 2)
(define GDBM_NEWDB 3)
(define GDBM_SYNC #x20)
(define GDBM_NOLOCK #x40)
(define GDBM_NOMMAP #x80)

;;; db procedures

(define* (gdbm-open path flags #:key (mode #o666) (block-size 512))
  (define-foreign %gdbm-open '* "gdbm_open" (list '* int int int '*))
  ;; currently doesn't provide option to specify fatal_thunk
  (let ((result (%gdbm-open (string->pointer path)
                            block-size
                            flags
                            mode
                            %null-pointer)))
    (when (null-pointer? result)
      (gdbm-error))
    (wrap-db result)))

(define (gdbm-close db)
  (%gdbm-close (unwrap-db db)))
