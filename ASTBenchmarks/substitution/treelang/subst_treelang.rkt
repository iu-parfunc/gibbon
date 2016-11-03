#lang s-exp "../../../TreeLang/treelang.rkt"

(require "../../grammar_racket.sexp")
(provide subst)

;; copied exactly


(define (subst [old : Sym] [new : Sym] [e0 : Toplvl]) : Toplvl
  (top old new e0))

;; (data Toplvl
;;       [DefineValues   (Listof Sym) Expr]
;;       [DefineSyntaxes (Listof Sym) Expr]
;;       [Expression Expr])
(define (top [old : Sym] [new : Sym] [e : Toplvl]) : Toplvl
  (case e
    [(DefineValues ls e)
     (DefineValues ls (expr old new e))]
    [(DefineSyntaxes ls e)
     (DefineSyntaxes ls (expr old new e))]
    [(BeginTop ls)
     (BeginTop (top-ls old new ls))]
    [(Expression e)
     (Expression (expr old new e))]))

(define (top-ls [old : Sym] [new : Sym] [es : ListToplvl]) : ListToplvl
  (case es
    [(CONSTOPLVL e es) (CONSTOPLVL (top old new e) (top-ls old new es))]
    [(NULLTOPLVL) (NULLTOPLVL)]))

(define (expr-ls [old : Sym] [new : Sym] [es : ListExpr]) : ListExpr
  (case es
    [(CONSEXPR e es) (CONSEXPR (expr old new e) (expr-ls old new es))]
    [(NULLEXPR) (NULLEXPR)]))

(define (memq [v : Sym] [ls : ListSym]) : Bool
  (case ls
    [(CONSSYM s ls) (or (eq? v s) (memq v ls))]
    [(NULLSYM) False]))

(define (bound? [old : Sym] [ls : LVBIND]) : Bool
  (case ls
    [(CONSLVBIND syms e rest) (if (memq old syms)
                                  True
                                  (bound? old rest))]
    [(NULLLVBIND) False]))
(define (subst-lvbind [old : Sym] [new : Sym] [lv : LVBIND]) : LVBIND
  (case lv
    [(NULLLVBIND) (NULLLVBIND)]
    [(CONSLVBIND syms e rest)
     (CONSLVBIND syms (expr old new e) (subst-lvbind old new rest))]))
(define (subst-lambdacase [old : Sym] [new : Sym] [lc : LAMBDACASE]) : LAMBDACASE
       (case lc
         [(NULLLAMBDACASE) lc]
         [(CONSLAMBDACASE formals exprs rest)
          (if (bound-in? old formals)
              (CONSLAMBDACASE formals exprs (subst-lambdacase old new rest))
              (CONSLAMBDACASE formals (expr-ls old new exprs) (subst-lambdacase old new rest)))]))

(define (expr [old : Sym] [new : Sym] [e : Expr]) : Expr
  (case e
    ;; Variable references:
    [(VARREF s)
     (if (eq? old s)
         (VARREF new)
         e)]
    [(Top s)
     (if (eq? old s)
         (Top new)
         e)]
    [(VariableReference s)   ; #%variable-reference
     (if (eq? old s)
         (VariableReference new)
         e)] 
    [(VariableReferenceTop s)   ; #%variable-reference (#%top . id)
     (if (eq? old s)
         (VariableReferenceTop new)
         e)]

    ;; Leaf forms:
    [(VariableReferenceNull)     ; (#%variable-reference)
     e]
    [(Quote d) e]
    [(QuoteSyntax d) e]
    [(QuoteSyntaxLocal d) e] ;; (quote-syntax datum #:local)

    ;; Binding forms:
    [(Lambda formals lse)
     (Lambda formals (if (bound-in? old formals)
                         lse
                         (expr-ls old new lse)))]
    [(CaseLambda cases)
     (CaseLambda (subst-lambdacase old new cases))]
    [(LetValues binds body)
     (if (bound? old binds)
         (LetValues (subst-lvbind old new binds) body)
         (LetValues (subst-lvbind old new binds) (expr-ls old new body)))]
    [(LetrecValues binds body)
     (if (bound? old binds)
         (LetrecValues binds body)
         (LetrecValues (subst-lvbind old new binds) (expr-ls old new body)))]
    [(If cond then else)
     (If (expr old new cond) (expr old new then) (expr old new else))]
    [(Begin exprs)
     (Begin (expr-ls old new exprs))]
    [(Begin0 e1 exprs)
     (Begin0 (expr old new e1) (expr-ls old new exprs))]
    [(App rator rands)  ;; (#%plain-app expr ...+)
     (App (expr old new rator) (expr-ls old new rands))]
    [(SetBang s e)
     (SetBang s (expr old new e))]
    [(WithContinuationMark e1 e2 e3)
     (WithContinuationMark (expr old new e1) (expr old new e2) (expr old new e3))]))

(define (bound-in? [sym : Sym] [formals : Formals]) : Bool
  (case formals
    [(F1 syms)
     (memq sym syms)]
    [(F2 syms s)
     (if (eq? sym s)
         True
         (memq sym syms))]
    [(F3 s)
     (eq? sym s)]))

