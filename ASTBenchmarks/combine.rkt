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
  (combine-all args outf))

(define (combine-all fs out)
  (define ls (map file->value fs))
  (define outp (if out (open-output-file out) (current-output-port)))
  (write
   `(BeginTop
     ,(for/fold ([v '(NULLTOPLVL)]) ([l ls])
        `(CONSTOPLVL ,l ,v)))
   outp)
  (newline outp))