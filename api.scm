(import
 (owl toplevel)
 (owl json)
 (owl regex)
 (prefix (owl sys) sys/)
 (prefix (only (robusta encoding json) encode) json/)
 (prefix (robusta encoding url) url/))

,load "kelp-config.scm"

(define cut-ext (string->regex "s/\\.(.*?)$//"))
(define version-file? (string->regex "m/^[0-9]+$/"))

(define (P* f . s)
  (for-each
   (λ (x) (write-bytes f (string->list (str x))))
   s))

(define (P . s)
  (apply P* (append (list stdout) s)))

(define (aq v l) (cdr* (assq v l)))
(define (chdir* s)
  (when (not (sys/directory? s))
    (sys/mkdir s #o777))
  (sys/chdir s))

(define (add-script)
  (let* ((data (url/decode-form (fold string-append "" (force-ll (lines stdin)))))
         (auth-key (aq 'auth data))
         (description (aq 'desc data))
         (filename (aq 'name data))
         (contents (aq 'script data))
         (author (cdr* (assoc auth-key *auth-keys* string=?))))
    (if author
        (let ((stamp (str (time-ms))))
          (chdir* (string-append *data-location* "/" filename))
          (let* ((names (list stamp (string-append stamp ".desc") (string-append stamp ".author")))
                 (dataf (open-output-file (lref names 0)))
                 (descf (open-output-file (lref names 1)))
                 (authf (open-output-file (lref names 2))))
            (P* dataf contents)
            (P* descf description)
            (P* authf author)
            (for-each close-port (list dataf descf authf))
            (for-each (λ (x) (sys/chmod x #o777 #t)) names)
            (P (json/encode '((ok . ok))))))
        (P "Unauthorized"))))

(define (versions-of dist)
  (if ((string->regex "m/\\.\\./") dist)
      '() ;; nice try
      (let ((fs (sys/dir->list (string-append *data-location* "/" dist))))
        (if fs
            (filter version-file? fs)
            '()))))

(define (list-scripts)
  (let ((dirs (sys/dir->list *data-location*)))
    (P (json/encode (map (λ (x)
                           (let* ((vs (map string->number (versions-of x)))
                                  (newest (number->string (car* (sort > vs))))
                                  (author (list->string (file->list (string-append *data-location* "/" x "/" newest ".author"))))
                                  (desc (list->string (file->list (string-append *data-location* "/" x "/" newest ".desc")))))
                             (cons x `((author . ,author)
                                       (description . ,desc)
                                       (versions . ,vs)))))
                         dirs)))))

(define (get-script data)
  (let* ((f (aq 'get data))
         (av (aq 'version data))
         (vs (versions-of f))
         (chosen (car* (sort > (map string->number vs)))))
    (if av
        (if (has? vs av)
            (write-bytes stdout (file->list (string-append *data-location* "/" f "/" av)))
            (P "this version is not available"))
        (if (not (null? chosen))
            (write-bytes stdout (file->list (string-append *data-location* "/" f "/" (number->string chosen))))
            (P "no such script")))))
(λ (_)
  (chdir* *data-location*)

  (if-lets ((q (sys/getenv "QUERY_STRING")))
    (cond
     ((string=? q "add")
      (P "Content-type: application/json\r\n\r\n")
      (add-script))
     ((string=? q "list")
      (P "Content-type: application/json\r\n\r\n")
      (list-scripts))
     (else
      (let ((data (url/decode-form (sys/getenv "QUERY_STRING"))))
        (cond
         ((aq 'get data)
          (P "Content-type: text/plain\r\n\r\n")
          (get-script data))
         (else
          (P "invalid request")))))))
  0)
