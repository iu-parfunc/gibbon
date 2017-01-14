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
           (App (VARREF configure) (CONSEXPR (Quote (INTLIT 5)) (NULLEXPR))))
          (NULLTOPLVL))))
       (NULLTOPLVL)))
     (CONSTOPLVL
      (BeginTop (NULLTOPLVL))
      (CONSTOPLVL
       (DefineSyntaxes
        (CONSSYM register-external-file (NULLSYM))
        (App
         (VARREF make-keyword-syntax)
         (CONSEXPR
          (Lambda
           (F1 (NULLSYM))
           (CONSEXPR
            (App
             (VARREF values)
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
         (CONSSYM register-external-file4 (NULLSYM))
         (Lambda
          (F1 (CONSSYM indirect1 (CONSSYM indirect2 (CONSSYM f3 (NULLSYM)))))
          (CONSEXPR
           (LetValues
            (CONSLVBIND (CONSSYM f (NULLSYM)) (VARREF f3) (NULLLVBIND))
            (CONSEXPR
             (LetValues
              (CONSLVBIND
               (CONSSYM indirect (NULLSYM))
               (If (VARREF indirect2) (VARREF indirect1) (Quote (INTLIT 5)))
               (NULLLVBIND))
              (CONSEXPR
               (LetValues
                (NULLLVBIND)
                (CONSEXPR
                 (App
                  (VARREF register-external)
                  (CONSEXPR
                   (Quote (INTLIT 5))
                   (CONSEXPR
                    (VARREF f)
                    (CONSEXPR
                     (Quote (INTLIT 5))
                     (CONSEXPR (VARREF indirect) (NULLEXPR))))))
                 (NULLEXPR)))
               (NULLEXPR)))
             (NULLEXPR)))
           (NULLEXPR))))
        (CONSTOPLVL
         (DefineValues
          (CONSSYM unpack5 (NULLSYM))
          (Lambda
           (F1 (CONSSYM given-kws (CONSSYM given-args (CONSSYM f3 (NULLSYM)))))
           (CONSEXPR
            (LetValues
             (CONSLVBIND
              (CONSSYM indirect2 (NULLSYM))
              (App (VARREF pair) (CONSEXPR (VARREF given-kws) (NULLEXPR)))
              (NULLLVBIND))
             (CONSEXPR
              (LetValues
               (CONSLVBIND
                (CONSSYM indirect1 (NULLSYM))
                (If
                 (VARREF indirect2)
                 (App (VARREF car) (CONSEXPR (VARREF given-args) (NULLEXPR)))
                 (App (VARREF void) (NULLEXPR)))
                (NULLLVBIND))
               (CONSEXPR
                (App
                 (VARREF register-external-file4)
                 (CONSEXPR
                  (VARREF indirect1)
                  (CONSEXPR
                   (VARREF indirect2)
                   (CONSEXPR (VARREF f3) (NULLEXPR)))))
                (NULLEXPR)))
              (NULLEXPR)))
            (NULLEXPR))))
         (CONSTOPLVL
          (DefineValues
           (CONSSYM register-external-file6 (NULLSYM))
           (App
            (VARREF make-optional-keyword-procedure)
            (CONSEXPR
             (Lambda
              (F1 (CONSSYM given-kws (CONSSYM given-argc (NULLSYM))))
              (CONSEXPR
               (If
                (App
                 (VARREF symbol-wiped-out-by-character-filtration)
                 (CONSEXPR
                  (VARREF given-argc)
                  (CONSEXPR (Quote (INTLIT 3)) (NULLEXPR))))
                (LetValues
                 (CONSLVBIND
                  (CONSSYM l1 (NULLSYM))
                  (VARREF given-kws)
                  (NULLLVBIND))
                 (CONSEXPR
                  (LetValues
                   (CONSLVBIND
                    (CONSSYM l1 (NULLSYM))
                    (If
                     (App (VARREF null) (CONSEXPR (VARREF l1) (NULLEXPR)))
                     (VARREF l1)
                     (If
                      (App
                       (VARREF eq)
                       (CONSEXPR
                        (App (VARREF car) (CONSEXPR (VARREF l1) (NULLEXPR)))
                        (CONSEXPR (Quote (INTLIT 5)) (NULLEXPR))))
                      (App (VARREF cdr) (CONSEXPR (VARREF l1) (NULLEXPR)))
                      (VARREF l1)))
                    (NULLLVBIND))
                   (CONSEXPR
                    (App (VARREF null) (CONSEXPR (VARREF l1) (NULLEXPR)))
                    (NULLEXPR)))
                  (NULLEXPR)))
                (Quote (INTLIT 5)))
               (NULLEXPR)))
             (CONSEXPR
              (CaseLambda
               (CONSLAMBDACASE
                (F1
                 (CONSSYM
                  given-kws
                  (CONSSYM given-args (CONSSYM f (NULLSYM)))))
                (CONSEXPR
                 (App
                  (VARREF unpack5)
                  (CONSEXPR
                   (VARREF given-kws)
                   (CONSEXPR
                    (VARREF given-args)
                    (CONSEXPR (VARREF f) (NULLEXPR)))))
                 (NULLEXPR))
                (NULLLAMBDACASE)))
              (CONSEXPR
               (VARREF null)
               (CONSEXPR
                (Quote (INTLIT 5))
                (CONSEXPR
                 (LetValues
                  (CONSLVBIND
                   (CONSSYM register-external-file (NULLSYM))
                   (CaseLambda
                    (CONSLAMBDACASE
                     (F1 (CONSSYM f (NULLSYM)))
                     (CONSEXPR
                      (App
                       (VARREF unpack5)
                       (CONSEXPR
                        (VARREF null)
                        (CONSEXPR
                         (VARREF null)
                         (CONSEXPR (VARREF f) (NULLEXPR)))))
                      (NULLEXPR))
                     (NULLLAMBDACASE)))
                   (NULLLVBIND))
                  (CONSEXPR (VARREF register-external-file) (NULLEXPR)))
                 (NULLEXPR))))))))
          (CONSTOPLVL
           (DefineSyntaxes
            (CONSSYM register-external-module (NULLSYM))
            (App
             (VARREF make-keyword-syntax)
             (CONSEXPR
              (Lambda
               (F1 (NULLSYM))
               (CONSEXPR
                (App
                 (VARREF values)
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
             (CONSSYM register-external-module10 (NULLSYM))
             (Lambda
              (F1
               (CONSSYM indirect7 (CONSSYM indirect8 (CONSSYM f9 (NULLSYM)))))
              (CONSEXPR
               (LetValues
                (CONSLVBIND (CONSSYM f (NULLSYM)) (VARREF f9) (NULLLVBIND))
                (CONSEXPR
                 (LetValues
                  (CONSLVBIND
                   (CONSSYM indirect (NULLSYM))
                   (If
                    (VARREF indirect8)
                    (VARREF indirect7)
                    (Quote (INTLIT 5)))
                   (NULLLVBIND))
                  (CONSEXPR
                   (LetValues
                    (NULLLVBIND)
                    (CONSEXPR
                     (App
                      (VARREF register-external)
                      (CONSEXPR
                       (Quote (INTLIT 5))
                       (CONSEXPR
                        (VARREF f)
                        (CONSEXPR
                         (Quote (INTLIT 5))
                         (CONSEXPR (VARREF indirect) (NULLEXPR))))))
                     (NULLEXPR)))
                   (NULLEXPR)))
                 (NULLEXPR)))
               (NULLEXPR))))
            (CONSTOPLVL
             (DefineValues
              (CONSSYM unpack11 (NULLSYM))
              (Lambda
               (F1
                (CONSSYM
                 given-kws
                 (CONSSYM given-args (CONSSYM f9 (NULLSYM)))))
               (CONSEXPR
                (LetValues
                 (CONSLVBIND
                  (CONSSYM indirect8 (NULLSYM))
                  (App (VARREF pair) (CONSEXPR (VARREF given-kws) (NULLEXPR)))
                  (NULLLVBIND))
                 (CONSEXPR
                  (LetValues
                   (CONSLVBIND
                    (CONSSYM indirect7 (NULLSYM))
                    (If
                     (VARREF indirect8)
                     (App
                      (VARREF car)
                      (CONSEXPR (VARREF given-args) (NULLEXPR)))
                     (App (VARREF void) (NULLEXPR)))
                    (NULLLVBIND))
                   (CONSEXPR
                    (App
                     (VARREF register-external-module10)
                     (CONSEXPR
                      (VARREF indirect7)
                      (CONSEXPR
                       (VARREF indirect8)
                       (CONSEXPR (VARREF f9) (NULLEXPR)))))
                    (NULLEXPR)))
                  (NULLEXPR)))
                (NULLEXPR))))
             (CONSTOPLVL
              (DefineValues
               (CONSSYM register-external-module12 (NULLSYM))
               (App
                (VARREF make-optional-keyword-procedure)
                (CONSEXPR
                 (Lambda
                  (F1 (CONSSYM given-kws (CONSSYM given-argc (NULLSYM))))
                  (CONSEXPR
                   (If
                    (App
                     (VARREF symbol-wiped-out-by-character-filtration)
                     (CONSEXPR
                      (VARREF given-argc)
                      (CONSEXPR (Quote (INTLIT 3)) (NULLEXPR))))
                    (LetValues
                     (CONSLVBIND
                      (CONSSYM l1 (NULLSYM))
                      (VARREF given-kws)
                      (NULLLVBIND))
                     (CONSEXPR
                      (LetValues
                       (CONSLVBIND
                        (CONSSYM l1 (NULLSYM))
                        (If
                         (App (VARREF null) (CONSEXPR (VARREF l1) (NULLEXPR)))
                         (VARREF l1)
                         (If
                          (App
                           (VARREF eq)
                           (CONSEXPR
                            (App
                             (VARREF car)
                             (CONSEXPR (VARREF l1) (NULLEXPR)))
                            (CONSEXPR (Quote (INTLIT 5)) (NULLEXPR))))
                          (App (VARREF cdr) (CONSEXPR (VARREF l1) (NULLEXPR)))
                          (VARREF l1)))
                        (NULLLVBIND))
                       (CONSEXPR
                        (App (VARREF null) (CONSEXPR (VARREF l1) (NULLEXPR)))
                        (NULLEXPR)))
                      (NULLEXPR)))
                    (Quote (INTLIT 5)))
                   (NULLEXPR)))
                 (CONSEXPR
                  (CaseLambda
                   (CONSLAMBDACASE
                    (F1
                     (CONSSYM
                      given-kws
                      (CONSSYM given-args (CONSSYM f (NULLSYM)))))
                    (CONSEXPR
                     (App
                      (VARREF unpack11)
                      (CONSEXPR
                       (VARREF given-kws)
                       (CONSEXPR
                        (VARREF given-args)
                        (CONSEXPR (VARREF f) (NULLEXPR)))))
                     (NULLEXPR))
                    (NULLLAMBDACASE)))
                  (CONSEXPR
                   (VARREF null)
                   (CONSEXPR
                    (Quote (INTLIT 5))
                    (CONSEXPR
                     (LetValues
                      (CONSLVBIND
                       (CONSSYM register-external-module (NULLSYM))
                       (CaseLambda
                        (CONSLAMBDACASE
                         (F1 (CONSSYM f (NULLSYM)))
                         (CONSEXPR
                          (App
                           (VARREF unpack11)
                           (CONSEXPR
                            (VARREF null)
                            (CONSEXPR
                             (VARREF null)
                             (CONSEXPR (VARREF f) (NULLEXPR)))))
                          (NULLEXPR))
                         (NULLLAMBDACASE)))
                       (NULLLVBIND))
                      (CONSEXPR (VARREF register-external-module) (NULLEXPR)))
                     (NULLEXPR))))))))
              (CONSTOPLVL
               (DefineValues
                (CONSSYM register-external (NULLSYM))
                (Lambda
                 (F1
                  (CONSSYM
                   who
                   (CONSSYM f (CONSSYM module (CONSSYM indirect (NULLSYM))))))
                 (CONSEXPR
                  (If
                   (If
                    (App (VARREF path) (CONSEXPR (VARREF f) (NULLEXPR)))
                    (App
                     (VARREF complete-path)
                     (CONSEXPR (VARREF f) (NULLEXPR)))
                    (Quote (INTLIT 5)))
                   (App (VARREF void) (NULLEXPR))
                   (LetValues
                    (NULLLVBIND)
                    (CONSEXPR
                     (App
                      (VARREF raise-type-error)
                      (CONSEXPR
                       (VARREF who)
                       (CONSEXPR
                        (Quote (INTLIT 5))
                        (CONSEXPR (VARREF f) (NULLEXPR)))))
                     (NULLEXPR))))
                  (CONSEXPR
                   (App
                    (VARREF log-message)
                    (CONSEXPR
                     (App (VARREF current-logger) (NULLEXPR))
                     (CONSEXPR
                      (Quote (INTLIT 5))
                      (CONSEXPR
                       (Quote (INTLIT 5))
                       (CONSEXPR
                        (App
                         (VARREF format)
                         (CONSEXPR
                          (Quote (INTLIT 5))
                          (CONSEXPR (VARREF f) (NULLEXPR))))
                        (CONSEXPR
                         (If
                          (VARREF indirect)
                          (App
                           (VARREF apply)
                           (CONSEXPR
                            (VARREF make-prefab-struct)
                            (CONSEXPR
                             (Quote (INTLIT 5))
                             (CONSEXPR
                              (App
                               (VARREF list)
                               (CONSEXPR
                                (VARREF f)
                                (CONSEXPR
                                 (VARREF module)
                                 (CONSEXPR (Quote (INTLIT 5)) (NULLEXPR)))))
                              (NULLEXPR)))))
                          (App
                           (VARREF apply)
                           (CONSEXPR
                            (VARREF make-prefab-struct)
                            (CONSEXPR
                             (Quote (INTLIT 5))
                             (CONSEXPR
                              (App
                               (VARREF list)
                               (CONSEXPR
                                (VARREF f)
                                (CONSEXPR (VARREF module) (NULLEXPR))))
                              (NULLEXPR))))))
                         (NULLEXPR)))))))
                   (NULLEXPR)))))
               (NULLTOPLVL)))))))))))))
   (NULLTOPLVL)))
