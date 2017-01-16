#lang gibbon

(require "../../../ASTBenchmarks/grammar_racket.gib")
(require "../../../ASTBenchmarks/treewalk/gibbon/treewalk_gibbon.gib")

;; This does something weird in packed:
(let ((x : Toplvl (iterate (treewalk (Expression (CaseLambda (NULLLAMBDACASE))))))) x)

; (treewalk (Expression (CaseLambda (NULLLAMBDACASE))))

; (VARREF (quote skip))
