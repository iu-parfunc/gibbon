#lang racket

(require racket/exn)
; (require "grammar_racket.gib")
(require "./common/racket/parse.rkt")

(provide (all-defined-out))

;; ----------------------------------------

               
;; expects "<infile> <outfile>" lines on stdin
;; runs until it gets no more such lines (EOF)
(module+ main
  (let ([errors 0])
    (let loop ()
      (define l (read-line))
      (if (eof-object? l)
          (printf "binary_pack.rkt: Reached EOF; done.\n")
          (match (string-split l)
            [(list infile outfile)
             (printf "Converting to binary: ~a ~a\n" infile outfile)
             (with-handlers ([(lambda (_) #t)
                              (lambda (e)
                                (printf "ERROR while converting:\n  ~a"
                                        (exn->string e))
                                (set! errors (add1 errors)))])
               (call-with-output-file outfile
                 (lambda (outp)                  
                   (parse-pack-write (file->value infile) outp))
                 #:mode 'binary))
             (loop)]
            )))
    (if (zero? errors)
        (printf "Completed all conversions without error.\n")
        (begin (printf "Encountered ~a errors while converting.  Failing job.\n" errors)
               (exit 1)))))
