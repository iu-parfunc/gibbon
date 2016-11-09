#! /usr/bin/env racket
#lang typed/racket

;; Run substitution on a SINGLE input file for a given symbol and num iterations.

(require "../../common/racket/parse.rkt"
         "subst_treelang.rkt"
         (only-in "../../grammar_racket.sexp" Toplvl))

(define-values (oldsym file iters)
  (match (current-command-line-arguments)
    [(vector o f i) (values (string->symbol o) f 
                            (cast (string->number i) Real))]
    [args (error "unexpected number of command line arguments, expected <symbol> <file> <iterations>, got:\n"
                 args)]))

;; copied exactly + type annotations
(printf "\n\nBenchmark: Substituting symbol ~a in file ~a for ~a iterations...\n" oldsym file iters)
(printf "============================================================\n")

(define ast : Toplvl
   (time (parse (read (open-input-file file)))))
(printf "Done ingesting AST.\n")

(define newsym (string->symbol (string-append (symbol->string oldsym) "99")))
(define-values (_ cpu real gc)
  (time-apply (lambda () (for ([_ (in-range iters)])
                           (subst oldsym newsym ast)))
              '()))
(define batchseconds (/ real 1000.0))
(printf "ITERS: ~a\n" iters)
(printf "BATCHTIME: ~a\n" (exact->inexact batchseconds))
(printf "MEANTIME: ~a\n" (/ batchseconds iters))
(printf "Done with substitution pass.\n")
