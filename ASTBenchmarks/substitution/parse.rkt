#lang typed/racket

(require "../grammar_racket.sexp")
(provide parse)

(: parse : (Any -> Toplvl))
(define (parse v)
  (match v
    [`(DefineValues (,x ...) ,(app parse-expr e))
     #:when  (andmap symbol? x)
     (DefineValues x e)]
    [`(DefineSyntaxes (,x ...) ,(app parse-expr e))
     #:when (andmap symbol? x)
     (DefineSyntaxes x e)]
    [`(BeginTop (,e ...)) (BeginTop (map parse e))]
    [`(Expression ,(app parse-expr e))
     (Expression e)]))

(: parse-formals : (Any -> Formals))
(define (parse-formals v)
  (match v
    [`(F1 (,x ...))
     #:when (andmap symbol? x)
     (F1 x)]
    [`(F2 (,x ...) ,(? symbol? y))
     #:when (andmap symbol? x)
     (F2 x y)]
    [`(F3 ,(? symbol? x))
     (F3 x)]))

(: parse-lambdacase : (Any -> LAMBDACASE))
(define (parse-lambdacase v)
  (match v
    [`(MKLAMBDACASE ,(app parse-formals f) (,e ...))
     (MKLAMBDACASE f (map parse-expr e))]))

(: parse-lvbind : (Any -> LVBIND))
(define (parse-lvbind v)
  (match v
    [`(MKLVBIND (,x ...) ,(app parse-expr e))
     #:when (andmap symbol? x)
     (MKLVBIND x e)]))

(: parse-datum : (Any -> Datum))
(define (parse-datum v)
  (match v
    [`(INTLIT ,(? exact-integer? i))
     (INTLIT i)]))

(: is=? (-> Any (-> Any Boolean)))
(define ((is=? s1) s2)
  (equal? s1 s2))

(define-match-expander =?
  (lambda (stx) (syntax-case stx ()
                  [(_ expected) (syntax/loc stx
                                  (? (is=? expected)))])))

(: parse-expr : (Any -> Expr))
(define (parse-expr v)
  (match v
    [`(,(=? 'VARREF) ,(? symbol? x)) (VARREF x)]
    [`(,(=? 'Lambda) ,(app parse-formals fs) (,e ...))
     (Lambda fs (map parse-expr e))]
    [`(,(=? 'CaseLambda) (,lc ...))
     (CaseLambda (map parse-lambdacase lc))]
    [`(,(=? 'If) ,(app parse-expr cond) ,(app parse-expr then) ,(app parse-expr else))
     (If cond then else)]
    [`(,(=? 'Begin) (,e ...))
     (Begin (map parse-expr e))]
    [`(,(=? 'Begin0) ,(app parse-expr e1) (,e ...))
     (Begin0 e1 (map parse-expr e))]
    [`(,(=? 'LetValues) (,lvs ...) (,e ...))
     (LetValues (map parse-lvbind lvs) (map parse-expr e))]
    [`(,(=? 'LetrecValues) (,lvs ...) (,e ...))
     (LetrecValues (map parse-lvbind lvs) (map parse-expr e))]
    [`(,(=? 'SetBang) ,(? symbol? x) ,(app parse-expr e))
     (SetBang x e)]
    [`(,(=? 'Quote) ,(app parse-datum d))
     (Quote d)]
    [`(,(=? 'QuoteSyntax) ,(app parse-datum d))
     (QuoteSyntax d)]
    [`(,(=? 'WithContinuationMark) ,(app parse-expr e1) ,(app parse-expr e2) ,(app parse-expr e3))
     (WithContinuationMark e1 e2 e3)]
    [`(,(=? 'App) (,e ...))
     (App (map parse-expr e))]
    [`(,(=? 'Top) ,(? symbol? x))
     (Top x)]
    [`(,(=? 'VariableReference) ,(? symbol? x))
     (VariableReference x)]
    [`(,(=? 'VariableReferenceTop) ,(? symbol? x))
     (VariableReferenceTop x)]
    [`(,(=? 'VariableReferenceNull))
     (VariableReferenceNull)]))

