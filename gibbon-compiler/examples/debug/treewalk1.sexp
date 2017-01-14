#lang gibbon

(require "../../../ASTBenchmarks/grammar_racket.sexp")
(require "../../../ASTBenchmarks/treewalk/gibbon/treewalk_gibbon.rkt")

(let ((x : Toplvl
         (treewalk
          (BeginTop
            (CONSTOPLVL
             (BeginTop
               (CONSTOPLVL
                (BeginTop
                  (CONSTOPLVL
                   (BeginTop
                     (CONSTOPLVL
                      (BeginTop (NULLTOPLVL))
                      (CONSTOPLVL
                       (Expression
                        (App (VARREF (quote configure)) (CONSEXPR (Quote (INTLIT 5)) (NULLEXPR))))
                       (NULLTOPLVL))))
                   (NULLTOPLVL)))
                (CONSTOPLVL
                 (BeginTop (NULLTOPLVL))
                 (CONSTOPLVL
                  (DefineSyntaxes
                    (CONSSYM (quote register-external-file) (NULLSYM))
                    (App
                     (VARREF (quote make-keyword-syntax))
                     (CONSEXPR
                      (Lambda
                       (F1 (NULLSYM))
                       (CONSEXPR
                        (App
                         (VARREF (quote values))
                         (CONSEXPR
                          (QuoteSyntax (INTLIT 5))
                          (CONSEXPR (QuoteSyntax (INTLIT 5)) (NULLEXPR))))
                        (NULLEXPR)))
                      (CONSEXPR
                       (Quote (INTLIT 1))
                       (CONSEXPR
                        (Quote (INTLIT 0))
                        (CONSEXPR
                         (Quote (INTLIT 5))
                         (CONSEXPR
                          (Quote (INTLIT 5))
                          (CONSEXPR (Quote (INTLIT 5)) (NULLEXPR)))))))))
                  (CONSTOPLVL
                   (DefineValues
                     (CONSSYM (quote register-external-file4) (NULLSYM))
                     (Lambda
                      (F1 (CONSSYM (quote indirect1) (CONSSYM (quote indirect2) (CONSSYM (quote f3) (NULLSYM)))))
                      (CONSEXPR
                       (LetValues
                        (CONSLVBIND (CONSSYM (quote f) (NULLSYM)) (VARREF (quote f3)) (NULLLVBIND))
                        (CONSEXPR
                         (LetValues
                          (CONSLVBIND
                           (CONSSYM (quote indirect) (NULLSYM))
                           (If (VARREF (quote indirect2)) (VARREF (quote indirect1)) (Quote (INTLIT 5)))
                           (NULLLVBIND))
                          (CONSEXPR
                           (LetValues
                            (NULLLVBIND)
                            (CONSEXPR
                             (App
                              (VARREF (quote register-external))
                              (CONSEXPR
                               (Quote (INTLIT 5))
                               (CONSEXPR
                                (VARREF (quote f))
                                (CONSEXPR
                                 (Quote (INTLIT 5))
                                 (CONSEXPR (VARREF (quote indirect)) (NULLEXPR))))))
                             (NULLEXPR)))
                           (NULLEXPR)))
                         (NULLEXPR)))
                       (NULLEXPR))))
                   (CONSTOPLVL
                    (DefineValues
                      (CONSSYM (quote unpack5) (NULLSYM))
                      (Lambda
                       (F1 (CONSSYM (quote given-kws) (CONSSYM (quote given-args) (CONSSYM (quote f3) (NULLSYM)))))
                       (CONSEXPR
                        (LetValues
                         (CONSLVBIND
                          (CONSSYM (quote indirect2) (NULLSYM))
                          (App (VARREF (quote pair)) (CONSEXPR (VARREF (quote given-kws)) (NULLEXPR)))
                          (NULLLVBIND))
                         (CONSEXPR
                          (LetValues
                           (CONSLVBIND
                            (CONSSYM (quote indirect1) (NULLSYM))
                            (If
                             (VARREF (quote indirect2))
                             (App (VARREF (quote car)) (CONSEXPR (VARREF (quote given-args)) (NULLEXPR)))
                             (App (VARREF (quote void)) (NULLEXPR)))
                            (NULLLVBIND))
                           (CONSEXPR
                            (App
                             (VARREF (quote register-external-file4))
                             (CONSEXPR
                              (VARREF (quote indirect1))
                              (CONSEXPR
                               (VARREF (quote indirect2))
                               (CONSEXPR (VARREF (quote f3)) (NULLEXPR)))))
                            (NULLEXPR)))
                          (NULLEXPR)))
                        (NULLEXPR))))
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
                     (CONSTOPLVL
                      (DefineSyntaxes
                        (CONSSYM (quote register-external-module) (NULLSYM))
                        (App
                         (VARREF (quote make-keyword-syntax))
                         (CONSEXPR
                          (Lambda
                           (F1 (NULLSYM))
                           (CONSEXPR
                            (App
                             (VARREF (quote values))
                             (CONSEXPR
                              (QuoteSyntax (INTLIT 5))
                              (CONSEXPR (QuoteSyntax (INTLIT 5)) (NULLEXPR))))
                            (NULLEXPR)))
                          (CONSEXPR
                           (Quote (INTLIT 1))
                           (CONSEXPR
                            (Quote (INTLIT 0))
                            (CONSEXPR
                             (Quote (INTLIT 5))
                             (CONSEXPR
                              (Quote (INTLIT 5))
                              (CONSEXPR (Quote (INTLIT 5)) (NULLEXPR)))))))))
                      (CONSTOPLVL
                       (DefineValues
                         (CONSSYM (quote register-external-module10) (NULLSYM))
                         (Lambda
                          (F1
                           (CONSSYM (quote indirect7) (CONSSYM (quote indirect8) (CONSSYM (quote f9) (NULLSYM)))))
                          (CONSEXPR
                           (LetValues
                            (CONSLVBIND (CONSSYM (quote f) (NULLSYM)) (VARREF (quote f9)) (NULLLVBIND))
                            (CONSEXPR
                             (LetValues
                              (CONSLVBIND
                               (CONSSYM (quote indirect) (NULLSYM))
                               (If
                                (VARREF (quote indirect8))
                                (VARREF (quote indirect7))
                                (Quote (INTLIT 5)))
                               (NULLLVBIND))
                              (CONSEXPR
                               (LetValues
                                (NULLLVBIND)
                                (CONSEXPR
                                 (App
                                  (VARREF (quote register-external))
                                  (CONSEXPR
                                   (Quote (INTLIT 5))
                                   (CONSEXPR
                                    (VARREF (quote f))
                                    (CONSEXPR
                                     (Quote (INTLIT 5))
                                     (CONSEXPR (VARREF (quote indirect)) (NULLEXPR))))))
                                 (NULLEXPR)))
                               (NULLEXPR)))
                             (NULLEXPR)))
                           (NULLEXPR))))
                       (CONSTOPLVL
                        (DefineValues
                          (CONSSYM (quote unpack11) (NULLSYM))
                          (Lambda
                           (F1
                            (CONSSYM
                             (quote given-kws)
                             (CONSSYM (quote given-args) (CONSSYM (quote f9) (NULLSYM)))))
                           (CONSEXPR
                            (LetValues
                             (CONSLVBIND
                              (CONSSYM (quote indirect8) (NULLSYM))
                              (App (VARREF (quote pair)) (CONSEXPR (VARREF (quote given-kws)) (NULLEXPR)))
                              (NULLLVBIND))
                             (CONSEXPR
                              (LetValues
                               (CONSLVBIND
                                (CONSSYM (quote indirect7) (NULLSYM))
                                (If
                                 (VARREF (quote indirect8))
                                 (App
                                  (VARREF (quote car))
                                  (CONSEXPR (VARREF (quote given-args)) (NULLEXPR)))
                                 (App (VARREF (quote void)) (NULLEXPR)))
                                (NULLLVBIND))
                               (CONSEXPR
                                (App
                                 (VARREF (quote register-external-module10))
                                 (CONSEXPR
                                  (VARREF (quote indirect7))
                                  (CONSEXPR
                                   (VARREF (quote indirect8))
                                   (CONSEXPR (VARREF (quote f9)) (NULLEXPR)))))
                                (NULLEXPR)))
                              (NULLEXPR)))
                            (NULLEXPR))))
                        (CONSTOPLVL
                         (DefineValues
                           (CONSSYM (quote register-external-module12) (NULLSYM))
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
                                        (App
                                         (VARREF (quote car))
                                         (CONSEXPR (VARREF (quote l1)) (NULLEXPR)))
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
                                  (VARREF (quote unpack11))
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
                                   (CONSSYM (quote register-external-module) (NULLSYM))
                                   (CaseLambda
                                    (CONSLAMBDACASE
                                     (F1 (CONSSYM (quote f) (NULLSYM)))
                                     (CONSEXPR
                                      (App
                                       (VARREF (quote unpack11))
                                       (CONSEXPR
                                        (VARREF (quote null))
                                        (CONSEXPR
                                         (VARREF (quote null))
                                         (CONSEXPR (VARREF (quote f)) (NULLEXPR)))))
                                      (NULLEXPR))
                                     (NULLLAMBDACASE)))
                                   (NULLLVBIND))
                                  (CONSEXPR (VARREF (quote register-external-module)) (NULLEXPR)))
                                 (NULLEXPR))))))))
                         (CONSTOPLVL
                          (DefineValues
                            (CONSSYM (quote register-external) (NULLSYM))
                            (Lambda
                             (F1
                              (CONSSYM
                               (quote who)
                               (CONSSYM (quote f) (CONSSYM (quote module) (CONSSYM (quote indirect) (NULLSYM))))))
                             (CONSEXPR
                              (If
                               (If
                                (App (VARREF (quote path)) (CONSEXPR (VARREF (quote f)) (NULLEXPR)))
                                (App
                                 (VARREF (quote complete-path))
                                 (CONSEXPR (VARREF (quote f)) (NULLEXPR)))
                                (Quote (INTLIT 5)))
                               (App (VARREF (quote void)) (NULLEXPR))
                               (LetValues
                                (NULLLVBIND)
                                (CONSEXPR
                                 (App
                                  (VARREF (quote raise-type-error))
                                  (CONSEXPR
                                   (VARREF (quote who))
                                   (CONSEXPR
                                    (Quote (INTLIT 5))
                                    (CONSEXPR (VARREF (quote f)) (NULLEXPR)))))
                                 (NULLEXPR))))
                              (CONSEXPR
                               (App
                                (VARREF (quote log-message))
                                (CONSEXPR
                                 (App (VARREF (quote current-logger)) (NULLEXPR))
                                 (CONSEXPR
                                  (Quote (INTLIT 5))
                                  (CONSEXPR
                                   (Quote (INTLIT 5))
                                   (CONSEXPR
                                    (App
                                     (VARREF (quote format))
                                     (CONSEXPR
                                      (Quote (INTLIT 5))
                                      (CONSEXPR (VARREF (quote f)) (NULLEXPR))))
                                    (CONSEXPR
                                     (If
                                      (VARREF (quote indirect))
                                      (App
                                       (VARREF (quote apply))
                                       (CONSEXPR
                                        (VARREF (quote make-prefab-struct))
                                        (CONSEXPR
                                         (Quote (INTLIT 5))
                                         (CONSEXPR
                                          (App
                                           (VARREF (quote list))
                                           (CONSEXPR
                                            (VARREF (quote f))
                                            (CONSEXPR
                                             (VARREF (quote module))
                                             (CONSEXPR (Quote (INTLIT 5)) (NULLEXPR)))))
                                          (NULLEXPR)))))
                                      (App
                                       (VARREF (quote apply))
                                       (CONSEXPR
                                        (VARREF (quote make-prefab-struct))
                                        (CONSEXPR
                                         (Quote (INTLIT 5))
                                         (CONSEXPR
                                          (App
                                           (VARREF (quote list))
                                           (CONSEXPR
                                            (VARREF (quote f))
                                            (CONSEXPR (VARREF (quote module)) (NULLEXPR))))
                                          (NULLEXPR))))))
                                     (NULLEXPR)))))))
                               (NULLEXPR)))))
                          (NULLTOPLVL)))))))))))))
             (NULLTOPLVL)))
          )))
  ; True
  x
  )
