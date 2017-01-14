#lang gibbon

(require "../../../ASTBenchmarks/grammar_racket.sexp")
(require "../../../ASTBenchmarks/treewalk/gibbon/treewalk_gibbon.rkt")

(let ((x : Toplvl
         (treewalk
          (Expression
           (Begin (CONSEXPR
            (CaseLambda
             (CONSLAMBDACASE
              (F1
               (CONSSYM
                (quote given-kws)
                (CONSSYM (quote given-args) (CONSSYM (quote f) (NULLSYM)))))
              (CONSEXPR
               (App
                (VARREF (quote unpack5))
                (CONSEXPR
                 (VARREF (quote given-kws))
                 (CONSEXPR
                  (VARREF (quote given-args))
                  (CONSEXPR (VARREF (quote f)) (NULLEXPR)))))
               (NULLEXPR))
              (NULLLAMBDACASE)))
            (NULLEXPR)))
            )
          )))
  ; True
  x
  )
