#lang typed/racket

;; Infrastructure to set up and run the benchmark.

(require "parse.rkt"
         "subst_treelang.rkt"
         (only-in "../../grammar_racket.sexp" Toplvl))

(provide run-benchmarks)

(define oldsym 'call-with-values) ;; Hardcode this, doesn't matter.

(define (run-benchmarks [csv-port    : Output-Port]
                        [target-time : Real]
                        [dir         : Path-String]
                        [all-files   : (Listof Path-String)]
                        [skip-to     : Integer])
  (set! all-files (drop all-files skip-to))
  (define fileNum skip-to)

  (printf "Benchmark loop starting, processing ~a files and starting CSV output.\n" (length all-files))
  (fprintf csv-port "NAME, VARIANT, ARGS, ITERS, MEANTIME\n")
  (for ([relative (in-list all-files)])
    (define file (build-path dir relative))

    ;; copied exactly + type annotations
    (printf "\nBenchmark(~a): Substituting symbol ~a in file ~a ...\n" fileNum oldsym relative)
    (set! fileNum (add1 fileNum))
    (printf "============================================================\n")

    (define ast : Toplvl
      (time 
       (let* ([port (open-input-file file)]
              [res  (parse (read port))])
         (close-input-port port)
         res)))
    (printf "Done ingesting AST.\n")
    
    (let loop ([iters 1])
      (define newsym (string->symbol (string-append (symbol->string oldsym) "99")))
      (define-values (_ cpu real gc)
        (begin (collect-garbage) ;(collect-garbage)(collect-garbage)
               (time-apply (lambda () (for ([_ (in-range iters)])
                                        (subst oldsym newsym ast)))
                           '())))
      (define batchseconds (/ real 1000.0))

      (if (>= batchseconds target-time)
          (let ((meantime (exact->inexact (/ batchseconds iters))))
            (printf "\nITERATIONS: ~a\n" iters)
            (printf "BATCHTIME: ~a\n" (exact->inexact batchseconds))
            (printf "MEANTIME: ~a\n" meantime)
            (printf "Done with substitution pass.\n")

            (fprintf csv-port "substitution, treelang-racket, ~a, ~a, ~a\n"
                     relative iters meantime)
            (flush-output csv-port)
            (printf "Output written to: ~a\n" csv-port)
            )
          (begin (printf "~a " batchseconds) (flush-output)
                 (loop (* 2 iters)))))
    ))
