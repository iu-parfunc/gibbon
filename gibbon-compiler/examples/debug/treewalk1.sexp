#lang gibbon

(require "../../../ASTBenchmarks/grammar_racket.sexp")
(require "../../../ASTBenchmarks/treewalk/gibbon/treewalk_gibbon.rkt")

(let ((x : Toplvl
         (treewalk
          (Expression
           (CaseLambda
             (CONSLAMBDACASE
              (F1
               (CONSSYM
                (quote given-kws)
                (CONSSYM (quote given-args) (CONSSYM (quote f) (NULLSYM)))))
              (CONSEXPR
               (VARREF (quote skip))
               (NULLEXPR))
              (NULLLAMBDACASE)))
            )
          )))
  ; True
  x
  )

; (VARREF (quote skip))
