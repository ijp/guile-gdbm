(use-modules (gdbm)
             (ice-9 match)
             (web server)
             (web request)
             (web response)
             (rnrs bytevectors)
             (sxml simple)
             (web uri))

;;; helpers

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))
(define (redirect path)
  ;; using default host and port
  (let ((uri (build-uri 'http #:host "localhost" #:port 8080 #:path path)))
    (values (build-response #:code 302
                            #:headers `((location . ,uri)))
            "dummy"))) ; body seems to be necessary?

(define (query-parameters query-string)
  ;; TODO: doesn't handle #\; or do any error handling
  (map (lambda (fv-pair)
         (let ((decoded (map uri-decode (string-split fv-pair #\=))))
           (cons (string->symbol (car decoded))
                 (cadr decoded))))
       (string-split query-string #\&)))

;;; db operations

(define db (gdbm-open "/tmp/poll.db" GDBM_WRITER))

(define (alist-cons a b c)
  (cons (cons a b) c))

(define (get-results)
  (gdbm-fold alist-cons '() db))

(define (get-options)
  (gdbm-fold (lambda (k _ o)
             (cons k o))
           '()
           db))

(define (vote option)
  (let ((current (gdbm-ref db option #f)))
    (unless current
      (throw 'bad-vote))
    (gdbm-set! db option (string+1 current))))

(define string+1
  (compose number->string 1+ string->number))

;;; application

(define (voting-handler request body)
  (let* ((str (utf8->string body))
         (params (query-parameters str)))
    (and=> (assoc 'choice params)
           (lambda (pair)
             (vote (cdr pair))))
    ;; currently ignore bad requests
    (redirect "/results")))

(define (main-handler request body)
  (values '((content-type . (text/html)))
          (lambda (port)
            (sxml->xml
             `(form
               (@ (action . "vote") (method . "POST"))
               ,(map (lambda (x)
                       `(input (@ (name . "choice")
                                  (value . ,x)
                                  (type  . "radio"))
                               ,x))
                     (get-options))
               (input (@ (type . "submit") (value . "Vote"))))
             port))))

(define (results-handler request body)
  (values '((content-type . (text/html)))
          (lambda (port)
            (sxml->xml
             `(table
               (tr (th "Project") (th "Votes"))
               ,(map (lambda (x)
                       `(tr (td ,(car x)) (td ,(cdr x))))
                     (get-results)))
             port))))

(define (handler request body)
  (match (cons (request-method request) (request-path-components request))
    [('GET)
     (main-handler request body)]
    [('POST "vote")
     (voting-handler request body)]
    [('GET "results")
     (results-handler request body)]
    [else
     (not-found request)]))

(run-server handler)
