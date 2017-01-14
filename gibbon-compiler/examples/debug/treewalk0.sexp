#lang gibbon
(require "../../../ASTBenchmarks/grammar_racket.sexp")
(require "../../../ASTBenchmarks/treewalk/gibbon/treewalk_gibbon.rkt")

;; This passes in --pointer but gives an type error in --packed:
(let ((x : Toplvl (treewalk (Expression (CaseLambda (NULLLAMBDACASE)))))) 0)
