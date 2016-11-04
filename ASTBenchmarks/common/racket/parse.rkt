#lang typed/racket

;; An SExp -> treelang datatype (structs) parser hardcoded to the
;; grammar_racket.sexp language.

(require "../../grammar_racket.sexp")
(provide parse)

(: parse-toplvl-list : Any -> ListToplvl)
(define (parse-toplvl-list v)
  (match v
    [`(NULLTOPLVL) (NULLTOPLVL)]
    [`(CONSTOPLVL ,(app parse t) ,(app parse-toplvl-list ts)) (CONSTOPLVL t ts)]))

(: parse : (Any -> Toplvl))
(define (parse v)
  (match v
    [`(DefineValues ,x ,(app parse-expr e))
     (DefineValues (parse-listsym x) e)]
    [`(DefineSyntaxes ,x ,(app parse-expr e))
     (DefineSyntaxes (parse-listsym x) e)]
    [`(BeginTop ,e)
     (BeginTop (parse-toplvl-list e))]
    [`(Expression ,(app parse-expr e))
     (Expression e)]))

(: parse-formals : (Any -> Formals))
(define (parse-formals v)
  (match v
    [`(F1 ,(app parse-listsym x))
     (F1 x)]
    [`(F2 ,(app parse-listsym x) ,(? symbol? y))
     (F2 x y)]
    [`(F3 ,(? symbol? x))
     (F3 x)]))

(: parse-lambdacase : (Any -> LAMBDACASE))
(define (parse-lambdacase v)
  (match v
    [`(CONSLAMBDACASE ,(app parse-formals f) ,e ,rest)
     (CONSLAMBDACASE f (parse-expr-list e) (parse-lambdacase rest))]
    ['(NULLLAMBDACASE) (NULLLAMBDACASE)]))

(: parse-listsym : Any -> ListSym)
(define (parse-listsym v)
  (match v
    [`(CONSSYM ,(? symbol? s) ,rest) (CONSSYM s (parse-listsym rest))]
    ['(NULLSYM) (NULLSYM)]))

(: parse-lvbind : (Any -> LVBIND))
(define (parse-lvbind v)
  (match v
    [`(CONSLVBIND ,(app parse-listsym x) ,(app parse-expr e) ,rest)
     (CONSLVBIND x e (parse-lvbind rest))]
    ['(NULLLVBIND) (NULLLVBIND)]))

(: parse-datum : (Any -> Datum))
(define (parse-datum v)
  (match v
    [`(INTLIT ,(? fixnum? i))
     (INTLIT i)]))

(: is=? (-> Any (-> Any Boolean)))
(define ((is=? s1) s2)
  (equal? s1 s2))

(define-match-expander =?
  (lambda (stx) (syntax-case stx ()
                  [(_ expected) (syntax/loc stx
                                  (? (is=? expected)))])))

(: parse-expr-list : Any -> ListExpr)
(define (parse-expr-list e)
  (match e
    [`(NULLEXPR) (NULLEXPR)]
    [`(CONSEXPR ,(app parse-expr e) ,(app parse-expr-list rest))
     (CONSEXPR e rest)]))

(: parse-expr : (Any -> Expr))
(define (parse-expr v)
  (match v
    [`(,(=? 'VARREF) ,(? symbol? x)) (VARREF x)]
    [`(,(=? 'Lambda) ,(app parse-formals fs) ,e)
     (Lambda fs (parse-expr-list e))]
    [`(,(=? 'CaseLambda) ,lc)
     (CaseLambda (parse-lambdacase lc))]
    [`(,(=? 'If) ,(app parse-expr cond) ,(app parse-expr then) ,(app parse-expr else))
     (If cond then else)]
    [`(,(=? 'Begin) ,e)
     (Begin (parse-expr-list e))]
    [`(,(=? 'Begin0) ,(app parse-expr e1) ,e)
     (Begin0 e1 (parse-expr-list e))]
    [`(,(=? 'LetValues) ,lvs ,e)
     (LetValues (parse-lvbind lvs) (parse-expr-list e))]
    [`(,(=? 'LetrecValues) ,lvs ,e)
     (LetrecValues (parse-lvbind lvs) (parse-expr-list e))]
    [`(,(=? 'SetBang) ,(? symbol? x) ,(app parse-expr e))
     (SetBang x e)]
    [`(,(=? 'Quote) ,(app parse-datum d))
     (Quote d)]
    [`(,(=? 'QuoteSyntax) ,(app parse-datum d))
     (QuoteSyntax d)]
    [`(,(=? 'QuoteSyntaxLocal) ,(app parse-datum d))
     (QuoteSyntaxLocal d)]
    [`(,(=? 'WithContinuationMark) ,(app parse-expr e1) ,(app parse-expr e2) ,(app parse-expr e3))
     (WithContinuationMark e1 e2 e3)]
    [`(,(=? 'App) ,e0 ,es)
     (App (parse-expr e0) (parse-expr-list es))]
    [`(,(=? 'Top) ,(? symbol? x))
     (Top x)]
    [`(,(=? 'VariableReference) ,(? symbol? x))
     (VariableReference x)]
    [`(,(=? 'VariableReferenceTop) ,(? symbol? x))
     (VariableReferenceTop x)]
    [`(,(=? 'VariableReferenceNull))
     (VariableReferenceNull)]))


(: parse-pack-write : Any Output-Port -> Any)
(define (parse-pack-write v port)
  (define bs (pack-Toplvl (parse v)))
  (displayln (bytes-length bs))
  (write-bytes bs port))

(module+ main
  (require racket/cmdline)
  (command-line #:args (#{in : String} #{out : String})
                (parse-pack-write (file->value in) (open-output-file out))))