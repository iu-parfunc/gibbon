#lang gibbon

(require "../../ASTBenchmarks/grammar_racket.sexp")
; (require "../../ASTBenchmarks/substitution/treelang/subst_gibbon.rkt")

(define (memq [v : Sym] [ls : ListSym]) : Bool
  (case ls
    [(CONSSYM s ls) (or (eq? v s) (memq v ls))]
    [(NULLSYM) False]))

(memq (quote hi)
      (CONSSYM (quote hi) (NULLSYM)))
