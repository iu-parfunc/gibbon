>>>
(MKPROG ((Expression (VARREF a))))
>>>
(MKPROG ((Expression (Lambda (F3 x) (VARREF x)))))
>>>
(MKPROG ((Expression (Lambda (F1 (x y)) (VARREF x)))))
>>>
(MKPROG ((Expression (Lambda (F2 (x y) z) (VARREF x)))))
>>>
(MKPROG ((Expression (Lambda (F3 x) (VARREF x))) (Expression (Lambda (F3 x) (VARREF x)))))
>>>
(MKPROG ((Expression (CaseLambda ((MKLAMBDACASE (a) ((VARREF x) (VARREF x))) (MKLAMBDACASE (a b c) ((VARREF x))))))))
>>>
(MKPROG ((Expression (If (VARREF x) (VARREF x) (VARREF x)))))
>>>
(MKPROG ((Expression (Begin ((If (VARREF x) (VARREF x) (VARREF x)) (VARREF x))))))
>>>
(MKPROG ((Expression (Begin0 (VARREF x) ((If (VARREF X) (VARREF x) (VARREF x)))))))
>>>
(MKPROG ((Expression (LetValues ((MKLVBIND (a b) (VARREF x)) (MKLVBIND (c) (Lambda (F1 (x y)) (VARREF x)))) ((VARREF x) (If (VARREF x) (VARREF x) (VARREF x))))))) 
>>>
(MKPROG ((Expression (LetRecValues ((MKLVBIND (a) (VARREF x)) (MKLVBIND (b) (VARREF a))) ((VARREF x) (VARREF a))))))
>>>
(MKPROG ((Expression (SetBang a (VARREF x)))))
>>>
(MKPROG ((Expression (Quote (INTLIT 43)))))
>>>
(MKPROG ((Expression (QuoteSyntax (INTLIT 43)))))
>>>
(MKPROG ((Expression (QuoteSyntaxLocal (INTLIT 43)))))
>>>
(MKPROG ((Expression (WithContinuationMark (VARREF x) (VARREF x) (VARREF x)))))
>>>
(MKPROG ((Expression (App ((Lambda (F3 x) (VARREF x)) (VARREF y))))))
>>>
(MKPROG ((Expression (Top a))))
>>>
(MKPROG ((Expression (VariableReference a))))
>>>
(MKPROG ((Expression (VariableReferenceTop a))))
>>>
(MKPROG ((Expression (VariableReferenceNull))))
>>>
(MKPROG ((DefineValues (a b) (VARREF y))))
>>>
(MKPROG ((DefineSyntaxes (a b) (VARREF y))))
>>>
(MKPROG
  (
    (DefineValues (fib)
      (Lambda (F1 (n))
        (If (App ((VARREF <=) (VARREF n) (Quote (INTLIT 2))))
          (Quote (INTLIT 1))
            (App ( (VARREF +)
              (App ((VARREF fib)
                (App ((VARREF -) (VARREF n) (Quote (INTLIT 1))))))
              (App ((VARREF fib)
                (App ((VARREF -) (VARREF n) (Quote (INTLIT 2)))))))))))
     ))

