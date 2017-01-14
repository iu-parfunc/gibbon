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
            (CONSEXPR
             (VARREF (quote null))
             (CONSEXPR
              (Quote (INTLIT 5))
              (CONSEXPR
               (LetValues
                (CONSLVBIND
                 (CONSSYM (quote register-external-file) (NULLSYM))
                 (CaseLambda
                  (CONSLAMBDACASE
                   (F1 (CONSSYM (quote f) (NULLSYM)))
                   (CONSEXPR
                    (App
                     (VARREF (quote unpack5))
                     (CONSEXPR
                      (VARREF (quote null))
                      (CONSEXPR
                       (VARREF (quote null))
                       (CONSEXPR (VARREF (quote f)) (NULLEXPR)))))
                    (NULLEXPR))
                   (NULLLAMBDACASE)))
                 (NULLLVBIND))
                (CONSEXPR (VARREF (quote register-external-file)) (NULLEXPR)))
               (NULLEXPR))))))
            )
          )))
  ; True
  x
  )
