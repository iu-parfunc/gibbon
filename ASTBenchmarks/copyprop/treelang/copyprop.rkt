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
               (for/list ([e : Expr body])
                 (expr e nenv))))]
    [(CaseLambda cases)
     (CaseLambda (for/list ([lc : LAMBDACASE cases])
                   (case lc
                     [(MKLAMBDACASE fs exprs)
                      (let ([nenv : Env (formals-ee fs env)])
                        (MKLAMBDACASE (formals-update fs nenv)
                                      (for/list : (Listof Expr) ([e : Expr exprs])
                                        (expr e nenv))))])))]
    [(LetValues binds body) 
     (let ([nenv : Env (for/fold : Env ([e env])
                                       ([b : LVBIND binds])
                         (lvbind-ee b e))])
       (LetValues binds (for/list ([e : Expr body])
                          (expr e nenv))))]
    [(LetrecValues binds body) ;; anything different here?
     (let ([nenv : Env (for/fold : Env ([e env])
                         ([b : LVBIND binds])
                         (lvbind-ee b e))])
       (LetValues binds (for/list ([e : Expr body])
                          (expr e nenv))))]
    [(If cond then else)
     (If (expr cond env) (expr then env) (expr else env))]
    [(Begin exprs)
     (Begin (for/list ([e : Expr exprs])
              (expr e env)))]
    [(Begin0 e1 exprs)
     (Begin0 (expr e1 env) (for/list ([e : Expr exprs])
                             (expr e env)))]
    [(App exprs)  ;; (#%plain-app expr ...+)
     (App (for/list ([e : Expr exprs])
            (expr e env)))]
    [(SetBang s e)
     (SetBang (lookup-env s env) (expr e env))]
    [(WithContinuationMark e1 e2 e3)
     (WithContinuationMark (expr e1 env) (expr e2 env) (expr e3 env))]))


(define (lvbind-ee [lvb : LVBIND] [env : Env]) : Env
  (case lvb
    [(MKLVBIND syms e) ;; syms is always length 1?
     (case e
       [(VARREF sym)
        (Extended (list-ref syms 1) sym env)]
       [(Top sym)
        (Extended (list-ref syms 1) sym env)]
       [(VariableReference sym)
        (Extended (list-ref syms 1) sym env)]
       [(VariableReferenceTop sym)
        (Extended (list-ref syms 1) sym env)])]))

(define (formals-ee [f : Formals] [env : Env]) : Env
  (case f
    [(F1 ls)
     (for/fold : Env ([e env])
                     ([s : Sym ls])
       (Extended s (gensym) e))]
    [(F2 ls s)
     (let ([nenv : Env (for/fold : Env ([e env])
                         ([s : Sym ls])
                         (Extended s (gensym) e))])
       (Extended s (gensym) nenv))]
    [(F3 s)
     (Extended s (gensym) env)]))

(define (formals-update [f : Formals] [env : Env]) : Formals
  (case f
    [(F1 ls)
     (F1 (for/list ([s : Sym ls])
           (lookup-env s env)))]
    [(F2 ls s)
     (F2 (for/list ([s : Sym ls])
           (lookup-env s env))
         (lookup-env s env))]
    [(F3 s)
     (F3 (lookup-env s env))]))