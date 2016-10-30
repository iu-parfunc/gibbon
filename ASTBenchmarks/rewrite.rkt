#lang racket

(define (datum e) `(INTLIT 5))

(define (xform-expression e)
  (define xf xform-expression)
  (match e
    [`(if ,e1 ,e2 ,e3) `(If ,(xf e1) ,(xf e2) ,(xf e3))]
    [`(set! ,x ,e) `(SetBang ,x ,(xf e))]
    [`(quote ,e) `(Quote ,(datum e))]
    [`(quote-syntax ,e #:local) `(QuoteSyntaxLocal ,(datum e))]
    [`(module ,m ,lang ,e ...) `(Begin ,(map xf e))]
    [`(module* ,m ,lang ,e ...) `(Begin ,(map xf e))]
    [`(#%top ,s) `(Top ,s)]
    [`(begin ,e ...) `(Begin ,(map xf e))]
    [`(begin0 ,e ...) `(Begin0 ,(map xf e))]
    [`(#%variable-reference) `(VariableReferenceNull)]
    [`(#%variable-reference (#%top . ,i)) `(VariableReferenceTop ,i)]
    [`(#%variable-reference ,i) `(VariableReference ,i)]
    [`(#%app ,e ...) `(App ,(map xf e))]
    [`(#%plain-app ,e ...) `(App ,(map xf e))]
    [`(let-values (,binds ...) ,body ...)
     `(LetValues ,(lvbind binds) ,(xf body))]
    [`(letrec-values (,binds ...) ,body ...)
     `(LetrecValues ,(lvbind binds) ,(map xf body))]
    [(? symbol?) `(VARREF ,e)]
    [`(lambda ,fmls ,body ...)
     `(Lambda ,(xform-fmls fmls) ,(map xf body))]
    [`(#%plain-lambda ,fmls ,body ...)
     `(Lambda ,(xform-fmls fmls) ,(map xf body))]
    [`(case-lambda ,cl ...)
     `(CaseLambda ,(for/list ([c (in-list cl)])
                     `(MKLAMBDACASE ,(xform-fmls (first c))
                                    ,(xf (second c)))))]
    ))

(define (lvbind l)
  (for/list ([c l])
    (match l
      [`[(,x ...) ,e]
       `(MKLVBIND ,x ,(xform-expression e))])))

(define (xform-fmls a)
  (match a
    [`(,fmls ...) `(F1 ,fmls)]
    [`(,fmls ... . ,(? symbol? x)) 
     `(F2 ,fmls ,x)]
    [(? symbol? x)
     `(F3 ,x)]))

(define (xform-top))
