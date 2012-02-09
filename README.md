# Guile bindings for GDBM

## Description
This module provides functions for guile to manipulated databases
using the [gdbm](http://www.gnu.org.ua/software/gdbm/) library.

## License

guile-gdbm is licensed under the GPL version 3, as is gdbm. See
COPYING for more details.

## Example

    (use-modules (gdbm))

    (define db (gdbm-open "/tmp/example.db" GDBM_WRCREAT))

    (gdbm-set! db "foo" "bar")
    (gdbm-set! db "baz" "quux")
    (gdbm-set! db "zot" "veeblefetzer")

    (write (gdbm-fold (lambda (key value old)
                        (cons (cons key value) old))
                      '()
                      db))
    ;; (("foo" . "bar") ("zot" . "veeblefetzer") ("baz" . "quux"))
