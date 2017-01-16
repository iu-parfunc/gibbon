#lang racket
(module+ main
  (require racket/cmdline)
  (define outf #f)
  (define args (command-line
                #:once-any
                ["--out" out "output-file"
                         (unless (equal? out "-")
                           (set! outf out))]
                #:args args args))
  (if (null? args)
      (combine-all
       (let loop ()
         (define l (read-line))
         (if (eof-object? l) '()
             (cons l (loop))))
       outf)
      (combine-all args outf)))

(define (combine-all fs out)
  (printf "Combining ~a files.\n" (length fs))
  (define ls (map file->value fs))
  (define outp (if out (open-output-file out) (current-output-port)))
  (write
   `(BeginTop
     ,(for/fold ([v '(NULLTOPLVL)]) ([l ls])
        `(CONSTOPLVL ,l ,v)))
   outp)
  (newline outp))
