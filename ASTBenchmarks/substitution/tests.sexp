>>>
(Expression (VARREF a))
>>>
(Expression (Lambda (F3 x) (VARREF x)))
>>>
(Expression (Lambda (F1 (x y)) (VARREF x)))
>>>
(Expression (Lambda (F2 (x y) z) (VARREF x)))
>>>
(Expression (CaseLambda ((MKLAMBDACASE (F3 a) ((VARREF x) (VARREF x))) (MKLAMBDACASE (F1 (a b c)) ((VARREF x))) 
                                  (MKLAMBDACASE (F2 (a) b) ((VARREF a) (VARREF b))))))
>>>
(Expression (If (VARREF x) (VARREF x) (VARREF x)))
>>>
(Expression (Begin ((If (VARREF x) (VARREF x) (VARREF x)) (VARREF x))))
>>>
(Expression (Begin0 (VARREF x) ((If (VARREF X) (VARREF x) (VARREF x)))))
>>>
(Expression (LetValues ((MKLVBIND (a b) (VARREF x)) 
                                 (MKLVBIND (c) (Lambda (F1 (x y)) (VARREF x)))) 
                      ((VARREF x) (If (VARREF x) (VARREF x) (VARREF x))))) 
>>>
(Expression (LetRecValues ((MKLVBIND (a) (VARREF x)) (MKLVBIND (b) (VARREF a))) ((VARREF x) (VARREF a))))
>>>
(Expression (SetBang a (VARREF x)))
>>>
(Expression (Quote (INTLIT 43)))
>>>
(Expression (QuoteSyntax (INTLIT 43)))
>>>
(Expression (QuoteSyntaxLocal (INTLIT 43)))
>>>
(Expression (WithContinuationMark (VARREF x) (VARREF x) (VARREF x)))
>>>
(Expression (App ((Lambda (F3 x) (VARREF x)) (VARREF y))))
>>>
(Expression (Top a))
>>>
(Expression (VariableReference a))
>>>
(Expression (VariableReferenceTop a))
>>>
(Expression (VariableReferenceNull))
>>>
(DefineValues (a b) (VARREF y))
>>>
(DefineSyntaxes (a b) (VARREF y))
>>>
(BeginTop ((DefineValues (a b) (VARREF y)) (Expression (Top a))))
>>>
(BeginTop 
  ((DefineValues (a b) (VARREF y)) 
   (Expression (Top a)) 
     (BeginTop 
      ((DefineValues (a b) (VARREF y)) (Expression (Top a))))))
>>>
(DefineValues (fib)
      (Lambda (F1 (n))
        (If (App ((VARREF <=) (VARREF n) (Quote (INTLIT 2))))
          (Quote (INTLIT 1))
            (App ( (VARREF +)
              (App ((VARREF fib)
                (App ((VARREF -) (VARREF n) (Quote (INTLIT 1))))))
              (App ((VARREF fib)
                (App ((VARREF -) (VARREF n) (Quote (INTLIT 2)))))))))))
