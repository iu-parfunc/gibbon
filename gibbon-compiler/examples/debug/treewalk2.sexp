#lang gibbon

(require "../../../ASTBenchmarks/grammar_racket.gib")
(require "../../../ASTBenchmarks/treewalk/gibbon/treewalk_gibbon.rkt")

;; This stack-overflows after calling a long long chain of unpack_ListSym:
(treewalk (Expression (CaseLambda (NULLLAMBDACASE))))
