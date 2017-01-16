#lang gibbon
(require "../../../ASTBenchmarks/grammar_racket.gib")
(require "../../../ASTBenchmarks/treewalk/gibbon/treewalk_gibbon.gib")

;; This passes in --pointer but gives an type error in --packed:
(let ((x : Toplvl (treewalk (Expression (CaseLambda (NULLLAMBDACASE)))))) 0)
