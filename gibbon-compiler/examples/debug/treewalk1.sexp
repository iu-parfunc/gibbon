#lang gibbon

(require "../../../ASTBenchmarks/grammar_racket.sexp")
(require "../../../ASTBenchmarks/treewalk/gibbon/treewalk_gibbon.rkt")

(let ((x : Toplvl
         (treewalk
          (BeginTop
            (CONSTOPLVL
                     (DefineValues
                       (CONSSYM (quote register-external-file6) (NULLSYM))
                       (App
                        (VARREF (quote make-optional-keyword-procedure))
                        (CONSEXPR
                         (Lambda
                          (F1 (CONSSYM (quote given-kws) (CONSSYM (quote given-argc) (NULLSYM))))
                          (CONSEXPR
                           (If
                            (App
                             (VARREF (quote symbol-wiped-out-by-character-filtration))
                             (CONSEXPR
                              (VARREF (quote given-argc))
                              (CONSEXPR (Quote (INTLIT 3)) (NULLEXPR))))
                            (LetValues
                             (CONSLVBIND
                              (CONSSYM (quote l1) (NULLSYM))
                              (VARREF (quote given-kws))
                              (NULLLVBIND))
                             (CONSEXPR
                              (LetValues
                               (CONSLVBIND
                                (CONSSYM (quote l1) (NULLSYM))
                                (If
                                 (App (VARREF (quote null)) (CONSEXPR (VARREF (quote l1)) (NULLEXPR)))
                                 (VARREF (quote l1))
                                 (If
                                  (App
                                   (VARREF (quote eq))
                                   (CONSEXPR
                                    (App (VARREF (quote car)) (CONSEXPR (VARREF (quote l1)) (NULLEXPR)))
                                    (CONSEXPR (Quote (INTLIT 5)) (NULLEXPR))))
                                  (App (VARREF (quote cdr)) (CONSEXPR (VARREF (quote l1)) (NULLEXPR)))
                                  (VARREF (quote l1))))
                                (NULLLVBIND))
                               (CONSEXPR
                                (App (VARREF (quote null)) (CONSEXPR (VARREF (quote l1)) (NULLEXPR)))
                                (NULLEXPR)))
                              (NULLEXPR)))
                            (Quote (INTLIT 5)))
                           (NULLEXPR)))
                         (CONSEXPR
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
                             (NULLEXPR))))))))
                    (NULLTOPLVL)))
          )))
  ; True
  x
  )
