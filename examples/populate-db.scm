(use-modules (gdbm)
             (srfi srfi-1))

(define db (gdbm-open "/tmp/poll.db" GDBM_NEWDB))

(define gnu-projects
  '(gcc gdb emacs guile bazaar bash octave zile ed coreutils))

(define initial-data
  (map (lambda (project)
         (cons project (random 256)))
       gnu-projects))

;; now, make guile the best :P
(and=> (assoc 'guile initial-data)
       (lambda (pair)
         (set-cdr! pair
                   (+ (cdr pair)
                      (fold (lambda (pair old-max)
                              (max (cdr pair) old-max))
                            0
                            initial-data)))))

(for-each (lambda (kv-pair)
            (gdbm-set! db
                       (symbol->string (car kv-pair))
                       (number->string (cdr kv-pair))))
          initial-data)

(gdbm-close db)
