#lang gibbon

e(require "../../ASTBenchmarks/grammar_racket.gib")
; (require "../../ASTBenchmarks/substitution/treelang/subst_gibbon.gib")

(define (memq [v : Sym] [ls : ListSym]) : Bool
  (case ls
    [(CONSSYM s ls) (or (eq? v s) (memq v ls))]
    [(NULLSYM) False]))

(memq (quote hi)
      (CONSSYM (quote hi) (NULLSYM)))
