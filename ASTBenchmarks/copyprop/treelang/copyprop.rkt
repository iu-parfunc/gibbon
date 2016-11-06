#lang s-exp "../../../TreeLang/treelang.rkt"

(require "../../grammar_racket.sexp" (only-in racket gensym))

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
       (CONSLAMBDACASE (formals-update fs nenv)
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

(define (lookup-env1 [syms : ListSym] [env : (SymDict Sym)]) : ListSym
  (case syms
    [(CONSSYM s rest)
     (CONSSYM (lookup env s)
      (lookup-env1 rest env))]
    [(NULLSYM)
     syms]))

(define (loop6 [ls : ListSym] [env : (SymDict Sym)]) : (SymDict Sym)
  (case ls
    [(CONSSYM s ls)
     (loop6 ls (insert env s (gensym)))]
    [(NULLSYM)
     env]))

(define (loop7 [ls : ListSym] [env : (SymDict Sym)]) : ListSym
  (case ls
    [(CONSSYM s ls)
     (CONSSYM (lookup env s) (loop7 ls env))]
    [(NULLSYM)
     ls]))
  
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
       (Lambda (formals-update formals nenv)
       	       (loop2 body nenv)))]
    [(CaseLambda cases)
     (CaseLambda (loop3 cases env))]
    [(LetValues binds body)
     (let ([nbinds : LVBIND (lvbind-update-rhs binds env)])	
       (let ([nenv : (SymDict Sym) (lvbind-ee nbinds env)])
         (LetValues (lvbind-update nbinds nenv)
       		    (loop2 body nenv))))]
    [(LetrecValues binds body) ;; anything different here?
     (let ([nenv : (SymDict Sym) (letrec-lvbind-ee1 binds env)])
       (let ([nbinds : LVBIND (letrec-lvbind-update binds nenv)])
         (let ([nenv : (SymDict Sym) (letrec-lvbind-ee2 nbinds nenv)])
	   (let ([nbinds : LVBIND (letrec-rhs-update nbinds nenv)])
	     (LetrecValues nbinds (loop2 body nenv))))))]
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

;; gensym formals and extend the env with the mapping
(define (formals-ee [f : Formals] [env : (SymDict Sym)]) : (SymDict Sym)
  (case f
   [(F1 ls)
    (loop6 ls env)]
   [(F2 ls s)
    (let ([nenv : (SymDict Sym) (loop6 ls env)])
      (insert nenv s (gensym)))]
   [(F3 s)
   (insert env s (gensym))]))

;; update formals to match new name in env
(define (formals-update [f : Formals] [env : (SymDict Sym)]) : Formals
  (case f
   [(F1 ls)
    (F1 (loop7 ls env))]
   [(F2 ls s)
    (F2 (loop7 ls env) (lookup env s))]
   [(F3 s)
    (F3 (lookup env s))]))

;; gensym let bindings and extend env with new mappings. 2 new mappings for each lvbind
(define (lvbind-ee [ls : LVBIND] [env : (SymDict Sym)]) : (SymDict Sym)
  (case ls
    [(CONSLVBIND syms e ls)
     (lvbind-ee ls (case e
               	     [(VARREF sym)
		      (let ([tmp : Sym (gensym)])
		        (insert (extend-env1 syms tmp env) tmp sym))]
                     [_ env]))]
    [(NULLLVBIND)
     env]))

(define (lvbind-update [ls : LVBIND] [env : (SymDict Sym)]) : LVBIND
  (case ls
    [(CONSLVBIND syms e ls)
     (CONSLVBIND (lookup-env1 syms env)
                 e ;; what about here?
		 (lvbind-update ls env))]
    [(NULLLVBIND)
     ls]))

(define (lvbind-update-rhs [ls : LVBIND] [env : (SymDict Sym)]) : LVBIND
  (case ls
    [(CONSLVBIND syms e ls)
     (CONSLVBIND syms (expr e env) (lvbind-update-rhs ls env))]
    [(NULLLVBIND)
     ls]))

(define (letrec-lvbind-ee1 [ls : LVBIND] [env : (SymDict Sym)]) : (SymDict Sym)
  (case ls
    [(CONSLVBIND syms e ls)
     (letrec-lvbind-ee1 ls (extend-env1 syms (gensym) env))]
    [(NULLLVBIND)
     env]))

(define (letrec-lvbind-update [ls : LVBIND] [env : (SymDict Sym)]) : LVBIND
  (case ls
   [(CONSLVBIND syms e ls)
    (CONSLVBIND (lookup-env1 syms env)
    		(expr e env)
		(letrec-lvbind-update ls env))]
   [(NULLLVBIND)
    ls]))

(define (letrec-lvbind-ee2 [ls : LVBIND] [env : (SymDict Sym)]) : (SymDict Sym)
  (case ls
    [(CONSLVBIND syms e ls)
     (letrec-lvbind-ee2 ls (case e
     			     [(VARREF sym)
			      (extend-env1 syms sym env)]
			     [_ env]))]
    [(NULLLVBIND)
     env]))

(define (letrec-rhs-update [ls : LVBIND] [env : (SymDict Sym)]) : LVBIND
  (case ls
    [(CONSLVBIND syms e ls)
     (CONSLVBIND syms (expr e env) (letrec-rhs-update ls env))]
    [(NULLLVBIND)
     ls]))

