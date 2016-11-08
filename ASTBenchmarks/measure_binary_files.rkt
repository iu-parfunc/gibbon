#lang racket



#;
(define all-files 
  (parameterize ([current-directory "./binary_racket"])
    (find-files (lambda (p) 
                  (equal? (path-get-extension p) #".sexp"))
                "."
                ; #:skip-filtered-directory? #f
                )))


(define all-files (file->lines "cleaned_list.txt"))

(parameterize ([current-directory "./binary_racket"])
  (printf "NAME,BYTES\n")
  (for ((f (in-list all-files)))    
    (printf "~a,~a\n" f (file-size f))))
