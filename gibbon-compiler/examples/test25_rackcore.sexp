#lang gibbon

(require "../../ASTBenchmarks/grammar_racket.sexp")


(let ([exp : Toplvl (BeginTop (CONSTOPLVL (Expression (VARREF (quote hello)))
                                          (NULLTOPLVL)))])
  3)
