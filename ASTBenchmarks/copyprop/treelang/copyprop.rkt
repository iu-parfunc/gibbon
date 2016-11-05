#lang s-exp "../../../TreeLang/treelang.rkt"

(require "../../grammar_racket.sexp"
         (only-in racket
                  gensym
                  list-ref))

(provide copyprop)

;; define env
(data Env
      [Empty]
      [Extended Sym Sym Env]) ;; sym1 is s sym2 is value. so replace sym1 with sym2

(define (copyprop [e : Toplvl]) : Toplvl
  (top e (Empty)))

(define (loop1 [ls : ListToplvl] [env : Env]) : ListToplvl
  (case ls
    [(CONSTOPLVL tl ls)
     (CONSTOPLVL (top tl env) (loop1 ls env))]
    [(NULLTOPLVL)
     ls]))

(define (loop2 [ls : ListExpr] [env : Env]): ListExpr
  (case ls
    [(CONSEXPR e ls)
     (CONSEXPR (expr e env) (loop2 ls env))]
    [(NULLEXPR)
     ls]))   

(define (loop3 [ls : LAMBDACASE] [env : Env]) : LAMBDACASE
  (case ls
    [(CONSLAMBDACASE fs le ls)
     (let ([nenv : Env (formals-ee fs env)])
       (CONSLAMBDACASE (formals-update fs nenv)
       		       (loop2 le nenv)
     		       (loop3 ls env)))]
    [(NULLLAMBDACASE)
     ls]))

(define (extend-env1 [syms : ListSym] [sym : Sym] [env : Env]) : Env
  (case syms
    [(CONSSYM s rest)
     (Extended s sym env)]
    [(NULLSYM)
     env]))

(define (loop4 [ls : LVBIND] [env : Env]) : Env
  (case ls
    [(CONSLVBIND syms e ls)
     (loop4 ls (case e
                 [(VARREF sym)
               	  (extend-env1 syms sym env)]
       	      	 [(Top sym)
               	  (extend-env1 syms sym env)]
       	      	 [(VariableReference sym)
               	  (extend-env1 syms sym env)]
       	      	 [(VariableReferenceTop sym)
               	  (extend-env1 syms sym env)]))]
    [(NULLLVBIND)
     env]))

(define (loop5 [ls : ListSym] [env : Env]) : ListSym
  (case ls
    [(CONSSYM s ls)
     (CONSSYM (lookup-env s env) (loop5 ls env))]
    [(NULLSYM)
     ls]))

(define (loop6 [ls : ListSym] [env : Env]) : Env
  (case ls
    [(CONSSYM s ls)
     (loop6 ls (Extended s (gensym) env))]
    [(NULLSYM)
     env]))
  
(define (top [e : Toplvl] [env : Env]) : Toplvl
  (case e
    [(DefineValues ls e)
     (DefineValues ls (expr e env))]
    [(DefineSyntaxes ls e)
     (DefineSyntaxes ls (expr e env))]
    [(BeginTop ls)
     (BeginTop (loop1 ls env))]
    [(Expression e)
     (Expression (expr e env))]))

(define (lookup-env [s : Sym] [env : Env]) : Sym
  (case env
    [(Empty) s]
    [(Extended s1 s2 env)
     (if (eq? s1 s)
         (lookup-env s2 env)
         (lookup-env s env))]))

(define (expr [e : Expr] [env : Env]) : Expr
  (case e
    ;; Variable references:
    [(VARREF s)
     (VARREF (lookup-env s env))] ;; here
    [(Top s)
     e]
    [(VariableReference s)   ; #%variable-reference
     (VariableReference (lookup-env s env))] ;; here
    [(VariableReferenceTop s)   ; #%variable-reference (#%top . id)
     (VariableReferenceTop (lookup-env s env))] ;; here?

    ;; Leaf forms:
    [(VariableReferenceNull)     ; (#%variable-reference)
     e]
    [(Quote d) e]
    [(QuoteSyntax d) e]
    [(QuoteSyntaxLocal d) e] ;; (quote-syntax datum #:local)

    ;; Binding forms:
    [(Lambda formals body)
     (let ([nenv : Env (formals-ee formals env)])
       (Lambda (formals-update formals nenv)
       	       (loop2 body env)))]
    [(CaseLambda cases)
     (CaseLambda (loop3 cases env))]
    [(LetValues binds body) 
     (let ([nenv : Env (loop4 binds env)])
       (LetValues binds (loop2 body nenv)))]
    [(LetrecValues binds body) ;; anything different here?
     (let ([nenv : Env (loop4 binds env)])
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
     (SetBang (lookup-env s env) (expr e env))]
    [(WithContinuationMark e1 e2 e3)
     (WithContinuationMark (expr e1 env) (expr e2 env) (expr e3 env))]))

(define (formals-ee [f : Formals] [env : Env]) : Env
  (case f
    [(F1 ls)
     (loop6 ls env)]
    [(F2 ls s)
     (let ([nenv : Env (loop6 ls env)])
       (Extended s (gensym) nenv))]
    [(F3 s)
     (Extended s (gensym) env)]))

(define (formals-update [f : Formals] [env : Env]) : Formals
  (case f
    [(F1 ls)
     (F1 (loop5 ls env))]
    [(F2 ls s)
     (F2 (loop5 ls env) (lookup-env s env))]
    [(F3 s)
     (F3 (lookup-env s env))]))