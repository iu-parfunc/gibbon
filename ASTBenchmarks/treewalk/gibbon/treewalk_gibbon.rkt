#lang gibbon

(require "../../grammar_racket.sexp")
(provide treewalk)

;; Identity function on ASTs:
(define (treewalk [e : Toplvl]) : Toplvl
  (case e
    [(DefineValues ls e)    (DefineValues (sym-ls ls) (expr e))]
    [(DefineSyntaxes ls e)  (DefineSyntaxes (sym-ls ls) (expr e))]
    [(BeginTop ls)          (BeginTop (top-ls ls))]
    [(Expression e)         (Expression (expr e))]))

(define (top-ls [es : ListToplvl]) : ListToplvl
  (case es
    [(CONSTOPLVL e es)  (CONSTOPLVL (treewalk e) (top-ls es))]
    [(NULLTOPLVL)       (NULLTOPLVL)]))

(define (expr-ls [es : ListExpr]) : ListExpr
  (case es
    [(CONSEXPR e es) (CONSEXPR (expr e) (expr-ls es))]
    [(NULLEXPR) (NULLEXPR)]))

(define (walk-lvbind [lv : LVBIND]) : LVBIND
  (case lv
    [(NULLLVBIND) (NULLLVBIND)]
    [(CONSLVBIND syms e rest)
     (CONSLVBIND (sym-ls syms) (expr e) (walk-lvbind rest))]))

(define (walk-lambdacase [lc : LAMBDACASE]) : LAMBDACASE
       (case lc
         [(NULLLAMBDACASE) lc]
         [(CONSLAMBDACASE formals exprs rest)
          (CONSLAMBDACASE (walk-formals formals) (expr-ls exprs) (walk-lambdacase rest))]))

(define (sym-ls [ls : ListSym]) : ListSym
  (case ls
    [(CONSSYM s ls) (CONSSYM s (sym-ls ls))]
    [(NULLSYM)      (NULLSYM)]))

(define (walk-formals [formals : Formals]) : Formals
  (case formals
    [(F1 syms)   (F1 (sym-ls syms))]
    [(F2 syms s) (F2 (sym-ls syms) s)]
    [(F3 s)      (F3 s)]))

(define (walk-datum [d : Datum]) : Datum
  (case d
    [(INTLIT i) (INTLIT i)]))

(define (expr [e : Expr]) : Expr
  (case e
    ;; Variable references:
    [(VARREF s)               (VARREF s)]
    [(Top s)                  (Top s)]
    [(VariableReference s)    (VariableReference s)] 
    [(VariableReferenceTop s) (VariableReferenceTop s)]

    ;; Leaf forms:
    [(VariableReferenceNull)  (VariableReferenceNull)]
    [(Quote d)                (Quote (walk-datum d))]
    [(QuoteSyntax d)          (QuoteSyntax (walk-datum d))]
    [(QuoteSyntaxLocal d)     (QuoteSyntaxLocal (walk-datum d))] ;; (quote-syntax datum #:local)

    ;; Binding forms:
    [(Lambda formals lse)      (Lambda (walk-formals formals) (expr-ls lse))]
    [(CaseLambda cases)        (CaseLambda (walk-lambdacase cases))]
    [(LetValues binds body)    (LetValues (walk-lvbind binds) (expr-ls body))]
    [(LetrecValues binds body) (LetrecValues (walk-lvbind binds) (expr-ls body))]

    [(If cond then else)       (If (expr cond) (expr then) (expr else))]

    [(Begin exprs)      (Begin  (expr-ls exprs))]
    [(Begin0 e1 exprs)  (Begin0 (expr e1) (expr-ls exprs))]
    [(App rator rands)  (App (expr rator) (expr-ls rands))]
    [(SetBang s e)      (SetBang s (expr e))]
    [(WithContinuationMark e1 e2 e3)
     (WithContinuationMark (expr e1) (expr e2) (expr e3))]))

