#lang gibbon

(require "../../ASTBenchmarks/grammar_racket.sexp")
(require "../../ASTBenchmarks/treewalk/gibbon/treewalk_gibbon.rkt")

; (walk-datum (INTLIT 3))

;; No vars so it PRINTS the same:
(let ([tmp : Toplvl
           (treewalk (BeginTop (CONSTOPLVL (Expression
                                            (Quote (INTLIT 33)))
                                           (NULLTOPLVL))))])
  True)
