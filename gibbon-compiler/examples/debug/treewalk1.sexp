#lang gibbon

(require "../../../ASTBenchmarks/grammar_racket.sexp")
(require "../../../ASTBenchmarks/treewalk/gibbon/treewalk_gibbon.rkt")

;; This does something weird in packed:
(let ((x : Toplvl (iterate (treewalk (Expression (CaseLambda (NULLLAMBDACASE))))))) x)

; (treewalk (Expression (CaseLambda (NULLLAMBDACASE))))

; (VARREF (quote skip))
