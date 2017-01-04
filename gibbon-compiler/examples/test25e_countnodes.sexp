#lang gibbon

(require "../../ASTBenchmarks/grammar_racket.sexp")
(require "../../ASTBenchmarks/countnodes/treelang/countnodes.rkt")

;; A small test:
(+ (datum   (INTLIT 3))
(+ (formals (F3 (quote hi)))
(+ (expr    (VARREF (quote hi)))
   (top (BeginTop (NULLTOPLVL)))
   )))

