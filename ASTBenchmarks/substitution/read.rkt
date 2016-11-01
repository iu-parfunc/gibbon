#! /usr/bin/env racket
#lang typed/racket

(require "parse.rkt"
         "subst_treelang.rkt"
         (only-in "../grammar_racket.sexp" Toplvl))

(define-values (oldsym file iters)
  (match (current-command-line-arguments)
    [(vector o f i) (values (string->symbol o) f (string->number i))]
    [args (error "unexpected number of command line arguments, expected <symbol> <file> <iterations>, got:\n"
                 args)]))

;; copied exactly + type annotations
(printf "\n\nBenchmark: Substituting symbol ~a in file ~a for ~a iterations...\n" oldsym file iters)
(printf "============================================================\n")

(define ast : Toplvl
   (time (parse (read (open-input-file file)))))
(printf "Done ingesting AST.\n")

(define newsym (string->symbol (string-append (symbol->string oldsym) "99")))
(time (for ([_ (in-range (cast iters Real))])
        (subst oldsym newsym ast)))
(printf "Done with substitution pass.\n")

