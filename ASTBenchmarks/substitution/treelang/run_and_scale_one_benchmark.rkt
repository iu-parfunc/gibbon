#! /usr/bin/env racket
#lang typed/racket

(require "parse.rkt"
         "subst_treelang.rkt"
         (only-in "../../grammar_racket.sexp" Toplvl))

(define oldsym 'call-with-values)

(define-values (dir relative)
  (match (current-command-line-arguments)
    [(vector d f) (values d f)]
    [args (error "unexpected number of command line arguments, expected <symbol> <file> <iterations>, got:\n"
                 args)]))

(define file (string-append dir "/" relative))

;; copied exactly + type annotations
(printf "\n\nBenchmark: Substituting symbol ~a in file ~a ...\n" oldsym file)
(printf "============================================================\n")

(define ast : Toplvl
   (time (parse (read (open-input-file file)))))
(printf "Done ingesting AST.\n")

(define outfile (format "results_~a.csv" (current-seconds)))
(define csv (open-output-file outfile #:exists 'replace))

(fprintf csv "NAME, VARIANT, ARGS, ITERS, MEANTIME\n")

(define destdir "~/results_backup/tree-velocity/")

(let loop ([iters 1])
  (define newsym (string->symbol (string-append (symbol->string oldsym) "99")))
  (define-values (_ cpu real gc)
    (begin (collect-garbage) ;(collect-garbage)(collect-garbage)
           (time-apply (lambda () (for ([_ (in-range iters)])
                                    (subst oldsym newsym ast)))
                       '())))
  (define batchseconds (/ real 1000.0))
  
  (if (>= batchseconds 1.0)
      (let ((meantime (exact->inexact (/ batchseconds iters))))
        (printf "\nITERATIONS: ~a\n" iters)
        (printf "BATCHTIME: ~a\n" (exact->inexact batchseconds))
        (printf "MEANTIME: ~a\n" meantime)
        (printf "Done with substitution pass.\n")
        
        (fprintf csv "substitution, treelang-racket, ~a, ~a, ~a\n"
                 relative iters meantime)
        (close-output-port csv)
        (printf "Output written to: ~a\n" outfile)
        (system (format "mkdir -p ~a" destdir))
        ; (copy-file outfile (string-append destdir outfile))
        (system (format "cp ~a ~a" outfile (string-append destdir outfile)))
        (printf "Output copied to ~a\n" destdir)
        )
      (begin (printf "~a " batchseconds) (flush-output)
             (loop (* 2 iters)))))


