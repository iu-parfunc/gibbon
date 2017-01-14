#lang gibbon

(require "../../../ASTBenchmarks/grammar_racket.sexp")
(require "../../../ASTBenchmarks/treewalk/gibbon/treewalk_gibbon.rkt")

(let ((x : Toplvl
         (treewalk
          (Expression
           (CaseLambda
             (CONSLAMBDACASE
              (F1 (NULLSYM))
              (NULLEXPR)
              (NULLLAMBDACASE)))
            ))))
  ; True
  x
  )

; (VARREF (quote skip))
