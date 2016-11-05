#lang s-exp "../../../TreeLang/treelang.rkt"

(require "../../grammar_racket.sexp")

(provide copyprop)

(define (copyprop [e : Toplvl]) : Toplvl
  (top e (empty-dict)))

(define (loop1 [ls : ListToplvl] [env : (SymDict Sym)]) : ListToplvl
  (case ls
    [(CONSTOPLVL tl ls)
     (CONSTOPLVL (top tl env) (loop1 ls env))]
    [(NULLTOPLVL)
     ls]))

(define (loop2 [ls : ListExpr] [env : (SymDict Sym)]): ListExpr
  (case ls
    [(CONSEXPR e ls)
     (CONSEXPR (expr e env) (loop2 ls env))]
    [(NULLEXPR)
     ls]))   

(define (loop3 [ls : LAMBDACASE] [env : (SymDict Sym)]) : LAMBDACASE
  (case ls
    [(CONSLAMBDACASE fs le ls)
     (let ([nenv : (SymDict Sym) (formals-ee fs env)])
       (CONSLAMBDACASE fs
       		       (loop2 le nenv)
     		       (loop3 ls env)))]
    [(NULLLAMBDACASE)
     ls]))

(define (extend-env1 [syms : ListSym] [sym : Sym] [env : (SymDict Sym)]) : (SymDict Sym)
  (case syms
    [(CONSSYM s rest)
     (insert env s sym)]
    [(NULLSYM)
     env]))

(define (loop4 [ls : LVBIND] [env : (SymDict Sym)]) : (SymDict Sym)
  (case ls
    [(CONSLVBIND syms e ls)
     (loop4 ls (case e
                 [(VARREF sym)
               	  (extend-env1 syms sym env)]
                 [_ env]))]
    [(NULLLVBIND)
     env]))

(define (loop6 [ls : ListSym] [env : (SymDict Sym)]) : (SymDict Sym)
  (case ls
    [(CONSSYM s ls)
     (loop6 ls (delete env s))]
    [(NULLSYM)
     env]))
  
(define (top [e : Toplvl] [env : (SymDict Sym)]) : Toplvl
  (case e
    [(DefineValues ls e)
     (DefineValues ls (expr e env))]
    [(DefineSyntaxes ls e)
     (DefineSyntaxes ls (expr e env))]
    [(BeginTop ls)
     (BeginTop (loop1 ls env))]
    [(Expression e)
     (Expression (expr e env))]))

(define (lookup-env [s : Sym] [env : (SymDict Sym)]) : Sym
  (lookup env s))

(define (expr [e : Expr] [env : (SymDict Sym)]) : Expr
  (case e
    ;; Variable references:
    [(VARREF s)
     (VARREF
      (if (has-key? env s)
          (lookup-env s env)
	  s))]
    [(Top s)
     e]
    [(VariableReference s)   ; #%variable-reference
     e]
    [(VariableReferenceTop s)   ; #%variable-reference (#%top . id)
     e]     
    ;; Leaf forms:
    [(VariableReferenceNull)     ; (#%variable-reference)
     e]
    [(Quote d) e]
    [(QuoteSyntax d) e]
    [(QuoteSyntaxLocal d) e] ;; (quote-syntax datum #:local)

    ;; Binding forms:
    [(Lambda formals body)
     (let ([nenv : (SymDict Sym) (formals-ee formals env)])
       (Lambda formals
       	       (loop2 body nenv)))]
    [(CaseLambda cases)
     (CaseLambda (loop3 cases env))]
    [(LetValues binds body) 
     (let ([nenv : (SymDict Sym) (loop4 binds env)])
       (LetValues binds (loop2 body nenv)))]
    [(LetrecValues binds body) ;; anything different here?
     (let ([nenv : (SymDict Sym) (loop4 binds env)])
       (LetrecValues binds (loop2 body nenv)))]
    [(If cond then else)
     (If (expr cond env) (expr then env) (expr else env))]
    [(Begin exprs)
     (Begin (loop2 exprs env))]
    [(Begin0 e1 exprs)
     (Begin0 (expr e1 env) (loop2 exprs env))]
    [(App e1 exprs)  ;; (#%plain-app expr ...+)
     (App (expr e1 env) (loop2 exprs env))]
    [(SetBang s e)
     (SetBang s (expr e env))]
    [(WithContinuationMark e1 e2 e3)
     (WithContinuationMark (expr e1 env) (expr e2 env) (expr e3 env))]))

(define (formals-ee [f : Formals] [env : (SymDict Sym)]) : (SymDict Sym)
  (case f
    [(F1 ls)
     (loop6 ls env)]
    [(F2 ls s)
     (let ([nenv : (SymDict Sym) (loop6 ls env)])
       (delete nenv s))]
    [(F3 s)
     (delete env s)]))