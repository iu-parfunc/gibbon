#lang gibbon

(require "../../../ASTBenchmarks/grammar_racket.sexp")
(require "../../../ASTBenchmarks/treewalk/gibbon/treewalk_gibbon.rkt")

;; If we don't print the answer, this passes with iterate but not without:
; (let ((x : Toplvl (treewalk (Expression (CaseLambda (NULLLAMBDACASE)))))) x)

(treewalk (Expression (CaseLambda (NULLLAMBDACASE))))

; (VARREF (quote skip))
