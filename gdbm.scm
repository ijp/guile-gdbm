(define-module (gdbm)
  #:use-modules (system foreign))

(define-syntax-rule (define-foreign name ret string-name args)
  (define name
    (pointer->procedure ret (dynamic-func string-name libgdbm) args)))

(define libgdbm (dynamic-link "libgdbm"))

(define-foreign %gdbm-open '* "gdbm_open" (list '* int int int '*))

(define-foreign %gdbm-close void "gdbm_close" '(*))

(define-foreign %gdbm-store int "gdbm_store" `(* * * ,int))

(define-foreign %gdbm-fetch '* "gdbm_fetch" '(* *))

(define-foreign %gdbm-delete int "gdbm_delete" '(* *))

(define-foreign %gdbm-first-key '* "gdbm_firstkey" '(*))

(define-foreign %gdbm-next-key '* "gdbm_nextkey" '(* *))

(define-foreign %gdbm-reorganize int "gdbm_reorganize" '(*))

(define-foreign %gdbm-sync void "gdbm_sync" '(*))

(define-foreign %gdbm-exists int "gdbm_exists" '(* *))

(define-foreign %gdbm-strerror '* "gdbm_strerror" (list int))

(define-foreign %gdbm-setopt int "gdbm_setopt" `(* ,int * ,int))

(define-foreign %gdbm-fdesc int "gdbm_fdesc" '(*))
