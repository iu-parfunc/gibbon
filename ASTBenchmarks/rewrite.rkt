#lang racket

(require racket/exn)

(define (datum e)
  (if (fixnum? e)
      `(INTLIT ,e)
      '(INTLIT 5)))

(define (xform-expression e)
  (define xf xform-expression)
  #;
  (when (pair? e)
    (newline)
    (print (car e))
    (newline))

  (match e
    [`(if ,e1 ,e2 ,e3)          `(If ,(xf e1) ,(xf e2) ,(xf e3))]
    [`(set! ,x ,e)              `(SetBang ,x ,(xf e))]
    [`(quote ,e)                `(Quote ,(datum e))]
    [`(quote-syntax ,e)         `(QuoteSyntax ,(datum e))]
    [`(quote-syntax ,e #:local) `(QuoteSyntaxLocal ,(datum e))]
    [`(module ,m ,lang ,e ...)  `(Begin ,(map xf e))]
    [`(module* ,m ,lang ,e ...) `(Begin ,(map xf e))]
    [`(#%top . ,s) #:when (symbol? s) `(Top ,s)] ;; RRN: fixed
    [`(begin ,e ...)            `(Begin ,(map xf e))]
    [`(begin0 ,e ...)           `(Begin0 ,(map xf e))]
    [`(#%variable-reference)    `(VariableReferenceNull)]
    [`(#%variable-reference (#%top . ,i)) `(VariableReferenceTop ,i)]
    [`(#%variable-reference ,i) `(VariableReference ,i)]
    [`(#%app ,e ...)            `(App ,(map xf e))]
    [`(#%plain-app ,e ...)      `(App ,(map xf e))]
    [`(let-values (,binds ...) ,body ...)
     `(LetValues ,(lvbind binds) ,(map xf body))]
    [`(letrec-values (,binds ...) ,body ...)
     `(LetrecValues ,(lvbind binds) ,(map xf body))] ;; RRN: fixed.
    [(? symbol?) `(VARREF ,e)]
    [`(lambda ,fmls ,body ...)
     `(Lambda ,(xform-fmls fmls) ,(map xf body))]
    [`(#%plain-lambda ,fmls ,body)
     `(Lambda ,(xform-fmls fmls) ,(xf body))]
    [`(#%expression ,e) (xf e)]
    [`(case-lambda ,cl ...)
     `(CaseLambda ,(for/list ([c (in-list cl)])
                     `(MKLAMBDACASE ,(xform-fmls (first c))
                                    ,(xf (second c)))))]
    [`(with-continuation-mark ,e1 ,e2 ,e3)
     `(WithContinuationMark ,(xf e1) ,(xf e2) ,(xf e3))]
    [(? symbol? s) `(VARREF ,s)]
    ))

(define (lvbind l)
  (for/list ([c (in-list l)])
    (match c
      [`((,x ...) ,e)
       `(MKLVBIND ,x ,(xform-expression e))])))

(define (xform-fmls a)
  (match a
    [`(,fmls ...) `(F1 ,fmls)]
    [`(,fmls ... . ,(? symbol? x)) 
     `(F2 ,fmls ,x)]
    [(? symbol? x)
     `(F3 ,x)]))

(define (xform-top e)
  (match e
    [(? eof-object?) #f]  ;; empty file
    [`(#%require ,_ ...) `(BeginTop)]
    [`(#%provide ,_ ...) `(BeginTop)]
    [`(#%declare ,_ ...) `(BeginTop)] ;; RRN added.
    [`(module ,m ,lang ,e ...) `(BeginTop ,(map xform-top e))]
    [`(module* ,m ,lang ,e ...) `(BeginTop ,(map xform-top e))]
    [`(begin ,e ...)
     `(BeginTop ,(map xform-top e))]
    [`(begin-for-syntax ,e ...)  ;; RRN added.
     `(BeginTop ,(map xform-top e))]
    [`(#%module-begin ,e ...)
     `(BeginTop ,(map xform-top e))]
    [`(#%plain-module-begin ,e ...)
     `(BeginTop ,(map xform-top e))]
    [`(define-values (,x ...) ,e)
     `(DefineValues ,x ,(xform-expression e))]
    [`(define-syntaxes (,x ...) ,e)
     `(DefineSyntaxes ,x ,(xform-expression e))]
    [e `(Expression ,(xform-expression e))]))

;; ----------------------------------------

(define (to-sexps s)
  (with-input-from-string s
    (lambda ()
      (let loop ()
        (define x (read))
        (if (eof-object? x)
            '()
            (cons x (loop)))))))

;; ----------------------------------------

;; expects "<infile> <outfile>" lines on stdin
(module+ main
  (let ([errors 0])
    (let loop ()
      (define l (read-line))
      (if (eof-object? l)
          (printf "rewrite.rkt: Reached EOF; done.\n")
          (match (map (lambda (s) (if (symbol? s) (symbol->string s) s))
                      (to-sexps l))
            [(list infile outfile)
             (printf "Converting: ~a ~a\n" infile outfile)
             (with-handlers ([(lambda (_) #t)
                              (lambda (e)
                                (printf "ERROR while converting:\n  ~a"
                                        (exn->string e))
                                (set! errors (add1 errors)))])
               (let* ((v (with-input-from-file infile read))
                      (new                   
                       (xform-top v)))
                 (when new
                   (with-output-to-file outfile
                     (lambda () (write new) (newline))))))
             (loop)]
            )))
    (if (zero? errors)
        (printf "Completed all conversions without error.\n")
        (begin (printf "Encountered ~a errors while converting.  Failing job.\n" errors)
               (exit 1)))
    ))
