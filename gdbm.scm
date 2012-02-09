;;; gdbm.scm --- A guile interface to gdbm

;; Copyright (C) 2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(define-module (gdbm)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
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
            gdbm-close
            gdbm-set!
            gdbm-ref
            gdbm-contains?
            gdbm-delete!
            gdbm-for-each
            gdbm-fold
            gdbm-reorganize
            gdbm-sync
            gdbm->fdes
            gdbm-export
            gdbm-import
            ))

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

(define-foreign %gdbm-exists int "gdbm_exists" (list '* datum))

(define-foreign %gdbm-strerror '* "gdbm_strerror" (list int))

(define-foreign %gdbm-setopt int "gdbm_setopt" `(* ,int * ,int))

(define-foreign %gdbm-fdesc int "gdbm_fdesc" '(*))

(define-foreign %gdbm-export int "gdbm_export" `(* * ,int ,int))

(define-foreign %gdbm-import int "gdbm_import" `(* * ,int))

;;; gdbm types

(define-record-type <gdbm-db>
  (%make-db pointer path closed? can-write?)
  gdbm-db?
  (pointer unwrap-db)
  (path db-path)
  (closed? db-closed? set-db-closed!)
  (can-write? db-can-write?))

(define (make-db pointer path can-write?)
  (%make-db pointer path #f can-write?))

(set-record-type-printer! <gdbm-db>
  (lambda (db port)
    (format port
            "#<gdbm-db ~a~a>"
            (db-path db)
            (if (db-closed? db) "(closed)" ""))))

(define maximum-int (- (expt 2 (* 8 (sizeof int))) 1))

(define (string->db-datum string)
  (let ((bv (string->utf8 string)))
    (when (> (bytevector-length bv) maximum-int)
      (error "string is too large for db"))
    (make-c-struct datum
                   (list (bytevector->pointer bv)
                         (bytevector-length bv)))))

(define* (db-datum->string db-datum #:key (free? #f))
  (let* ((struct (parse-c-struct db-datum datum))
         (bv-pointer (car struct))
         (bv-length (cadr struct)))
    (if (null-pointer? bv-pointer)
        #f
        (let* ((bv (pointer->bytevector bv-pointer bv-length))
               (str (utf8->string bv)))
          (when free?
            (free bv-pointer))
          str))))

(define (free-db-datum db-datum)
  (let* ((struct (parse-c-struct db-datum datum))
         (str (car struct)))
    (free str)))

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

;;; insert flags
(define GDBM_INSERT 0)
(define GDBM_REPLACE 1)

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
    (make-db result path (writeable? flags))))

(define (writeable? flags)
  (define GDBM_OPENMASK 7)
  (not (= GDBM_READER (logand GDBM_OPENMASK flags))))

(define (gdbm-close db)
  (%gdbm-close (unwrap-db db)))

(define* (gdbm-set! db key value #:key (replace? #t))
  ;; traditional scheme semantics is always replace
  (define flag (if replace? GDBM_REPLACE GDBM_INSERT))
  (define key-datum (string->db-datum key))
  (define value-datum (string->db-datum value))
  (case (%gdbm-store (unwrap-db db) key-datum value-datum flag)
    ((-1)
     (error "invalid data in key or value"))
    ((1)
     (error "data exists for this key, and called with #:replace? #f"))))

(define* (gdbm-ref db key #:optional (default #f))
  (let ((result (%gdbm-fetch (unwrap-db db) (string->db-datum key))))
    (or (db-datum->string result #:free? #t)
        default)))

(define (gdbm-contains? db key)
  (not (zero? (%gdbm-exists (unwrap-db db) (string->db-datum key)))))

(define (gdbm-delete! db key)
  (let ((result (%gdbm-delete (unwrap-db db) (string->db-datum key))))
    (unless (zero? result)
      ;; stub for now. Correct error handling will to be able to
      ;; determine whether db is a reader or a writer.
      *unspecified*)))

(define (gdbm-for-each proc db)
  (gdbm-fold (lambda (key value old)
               (proc key value))
             #f
             db)
  *unspecified*)

(define (gdbm-fold kons knil db)
  ;; not call/cc safe, nor should kons delete from the database
  (let ((db (unwrap-db db)))
    (let loop ((raw-key (%gdbm-first-key db)) (knil knil))
      (let ((key-str (db-datum->string raw-key)))
        (if key-str
            ;; since key is there, we assume we always get a successful response
            (let* ((val (db-datum->string (%gdbm-fetch db raw-key) #:free? #t))
                   (next-value (kons key-str val knil))
                   (next-key (%gdbm-next-key db raw-key)))
              (free-db-datum raw-key)
              (loop next-key next-value))
            knil)))))

(define (gdbm-reorganize db)
  (unless (zero? (%gdbm-reorganize (unwrap-db db)))
    (gdbm-error)))

(define (gdbm-sync db)
  (%gdbm-sync (unwrap-db db)))

(define (gdbm->fdes db)
  (%gdbm-fdesc (unwrap-db db)))

(define* (gdbm-export db file flag #:key (mode #o666))
  (define result
    (%gdbm-export (unwrap-db db) (string->pointer file) flag mode))
  (when (negative? result)
    (gdbm-error)))

(define* (gdbm-import db file #:key (replace? #t))
  (define flag (if replace? GDBM_REPLACE GDBM_INSERT))
  (define result (%gdbm-import (unwrap-db db) (string->pointer file) flag))
  (when (negative? result)
    (gdbm-error)))

