#lang s-exp "../../../Gibbon/gibbon.rkt"

(require "../../grammar_racket.sexp" (only-in racket gensym))

(provide copyprop)

(define (copyprop [e : Toplvl]) : Toplvl
  (let ([nexpr : Expr (top-pass1 e)])
    (let ([mut : (SymDict Bool) (top-pass2 nexpr (empty-dict))])
      (top-pass3 nexpr (empty-dict) mut))))

(define (loop1 [ls : ListToplvl]) : ListToplvl
  (case ls
    [(CONSTOPLVL tl ls)
     (CONSTOPLVL (top-pass1 tl) (loop1 ls))]
    [(NULLTOPLVL)
     ls]))

(define (loop-begintop [ls : ListTopvlvl] [mut : (SymDict Bool)]) : (SymDict Bool)
  (case ls
    [(CONSTOPLVL tl ls)
     (loop-begintop ls (pass2 tl mut))]
    [(NULLTOPLVL)
     mut]))

(define (loop-begintop2 [ls : ListToplvl] [mut : (SymDict Bool)]) : ListToplvl
  (case ls
    [(CONSTOPLVL tl ls)
     (CONSTOPLVL (top-pass3 tl mut) (loop-begintop2 ls mut))]
    [(NULLTOPLVL)
     ls]))

;; call pass1 on list of exprs
(define (loop2 [ls : ListExpr] [env : (SymDict Sym)]) : ListExpr
  (case ls
    [(CONSEXPR e ls)
     (CONSEXPR (pass1 e env) (loop2 ls env))]
    [(NULLEXPR)
     ls]))

(define (listexpr-pass3 [ls : ListExpr] [env : (SymDict Sym)] [mut : (SymDict Bool)]) : ListExpr
  (case ls
    [(CONSEXPR e ls)
     (CONSEXPR (pass3 e env mut)
               (listexpr-pass3 ls env mut))]
    [(NULLEXPR)
     ls]))

(define (listexpr-pass2 [ls : ListExpr] [mut : (SymDict Bool)]) : (SymDict Bool)
  (case ls
    [(CONSEXPR e ls)
     (listexpr-pass2 ls (pass2 e mut))]
    [(NULLEXPR)
     mut]))

(define (caselambda-pass2 [ls : LAMBDACASE] [mut : (SymDict Bool)]) : (SymDict Bool)
  (case ls
    [(CONSLAMBDACASE fs le ls)
     (caselambda-pass2 ls (listexpr-pass2 le mut))]
    [(NULLLAMBDACASE)
     mut]))

(define (caselambda-pass3 [ls : LAMBDACASE] [env : (SymDict Sym)] [mut : (SymDict Bool)]) : LAMBDACASE
  (case ls
    [(CONSLAMBDACASE fs le ls)
     (CONSLAMBDACASE fs (listexpr-pass3 le env mut)
                     (caselambda-pass3 ls env mut))]
    [(NULLLAMBDACASE)
     ls]))

;; rename each lambda's params
;; update params with new names
;; update body
;; loop
(define (loop3 [ls : LAMBDACASE] [env : (SymDict Sym)]) : LAMBDACASE
  (case ls
    [(CONSLAMBDACASE fs le ls)
     (let ([nenv : (SymDict Sym) (formals-ee fs env)])
       (CONSLAMBDACASE (formals-update fs nenv)
       		       (loop2 le nenv)
     		       (loop3 ls env)))]
    [(NULLLAMBDACASE)
     ls]))

;; updates 
(define (lookup-syms [syms : ListSym] [env : (SymDict Sym)]) : ListSym
  (case syms
    [(CONSSYM s rest)
     (CONSSYM (if (has-key? env s) 
                  (lookup env s)
                  s)
              (lookup-syms rest env))]
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
  
(define (top-pass1 [e : Toplvl]) : Toplvl
  (case e
    [(DefineValues ls e)
     (DefineValues ls (pass1 e (empty-dict)))]
    [(DefineSyntaxes ls e)
     (DefineSyntaxes ls (pass1 e (empty-dict)))]
    [(BeginTop ls)
     (BeginTop (loop1 ls))]
    [(Expression e)
     (Expression (pass1 e (empty-dict)))]))

(define (top-pass2 [e : Toplvl] [mut : (SymDict Bool)]) : (SymDict Bool)
  (case e
    [(DefineValues ls e)
     (pass2 e mut)]
    [(DefineSyntaxes ls e)
     (pass2 e mut)]
    [(BeginTop ls)
     (loop-begintop ls mut)]
    [(Expression e)
     (pass2 e mut)]))

(define (top-pass3 [e : Toplvl] [mut : (SymDict Bool)]) : Topvlv
  (case e
    [(DefineValues ls e)
     (DefineValues ls (pass3 e (empty-dict) mut))]
    [(DefineSyntaxes ls e)
     (DefineSyntaxes ls (pass3 e (empty-dict) mut))]
    [(BeginTop ls)
     (BeginTop (loop-begintop2 ls mut))]))

;; rename variables
(define (pass1 [e : Expr] [env : (SymDict Sym)]) : Expr
  (case e
    [(VARREF s)
     (VARREF (if (has-key? env s)
                 (lookup env s)
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
     (let ([nenv : (SymDict Sym) (formals-ee formals env)]) ;; rename the params
       (Lambda (formals-update formals nenv) ;; update params with new names
       	       (loop2 body nenv)))] ;; update body with new names

    [(CaseLambda cases)
     (CaseLambda (loop3 cases env))] ;; do the above, but for case lambda

    [(LetValues binds body)
     (let ([nenv : (SymDict Sym) (lvbind-rename binds env)])
       (let ([nbinds : LVBIND (lvbind-update-lhs binds nenv)])
         (let ([nbinds : LVBIND (lvbind-update-rhs binds env)]) ;; env because not a let*
           (LetValues nbinds (loop2 body nenv)))))]

    [(LetrecValues binds body) ;; anything different here? one thing
     (let ([nenv : (SymDict Sym) (lvbind-rename binds env)])
       (let ([nbinds : LVBIND (lvbind-update-lhs binds nenv)])
         (let ([nbinds : LVBIND (lvbind-update-rhs binds nenv)]) ;; nenv because recursive.
           (LetrecValues nbinds (loop2 body nenv)))))]

    [(If cond then else)
     (If (pass1 cond env) (pass1 then env) (pass1 else env))]
    [(Begin exprs)
     (Begin (loop2 exprs env))]
    [(Begin0 e1 exprs)
     (Begin0 (pass1 e1 env) (loop2 exprs env))]
    [(App e1 exprs)  ;; (#%plain-app expr ...+)
     (App (pass1 e1 env) (loop2 exprs env))]
    [(SetBang s e)
     (SetBang s (pass1 e env))]
    [(WithContinuationMark e1 e2 e3)
     (WithContinuationMark (pass1 e1 env) (pass1 e2 env) (pass1 e3 env))]))

;; gather list of mutated variables
(define (pass2 [e : Expr] [mut : (SymDict Bool)]) : (SymDict Bool)
  (case e
    [(VARREF s)
     mut]
    [(Top s)
     mut]
    [(VariableReference s)   ; #%variable-reference
     mut]
    [(VariableReferenceTop s)   ; #%variable-reference (#%top . id)
     mut]     
    ;; Leaf forms:
    [(VariableReferenceNull)     ; (#%variable-reference)
     mut]
    [(Quote d) mut]
    [(QuoteSyntax d) mut]
    [(QuoteSyntaxLocal d) mut] ;; (quote-syntax datum #:local)

    ;; Binding forms:
    [(Lambda formals body)
     (listexpr-pass2 body mut)]

    [(CaseLambda cases)
     (caselambda-pass2 cases env)]

    [(LetValues binds body)
     ;; need to look for setbang on rhs
     (listexpr-pass2 body (lvbind-pass2 binds mut))]
    
    [(LetrecValues binds body)
     (listexpr-pass2 body (lvbind-pass2 binds mut))]

    [(If cond then else)
     (let ([mut : (SymDict Bool) (pass2 cond mut)])
       (let ([mut : (SymDict Bool) (pass2 then mut)])
         (pass2 else mut)))]

    [(Begin exprs)
     (listexpr-pass2 exprs mut)]

    [(Begin0 e1 exprs)
     (listexpr-pass2 exprs (pass2 e1 mut))]

    [(App e1 exprs)  ;; (#%plain-app expr ...+)
     (listexpr-pass2 exprs (pass2 e1 mut))]

    [(SetBang s e)
     (insert mut s #t)]

    [(WithContinuationMark e1 e2 e3)
     (let ([mut : (SymDict Bool) (pass2 e1 mut)])
       (let ([mut : (SymDict Bool) (pass2 e2 mut)])
         (pass2 e3 mut)))]))

(define (pass3 [e : Expr] [env : (SymDict Sym)] [mut : (SymDict Bool)]) : Expr
  (case e
    [(VARREF s)
     (VARREF (if (has-key? mut s)
                 s
                 (if (has-key? env s)
                     (lookup env s)
                     s)))]
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
     (Lambda formals (listexpr-pass3 body env mut))]

    [(CaseLambda cases)
     (CaseLambda (caselambda-pass3 cases env mut))]

    [(LetValues binds body) 
     ;; extend environment. 
     (let ([nenv : (SymDict Sym) (letvalues-extend-env binds env)])
       (LetValues binds (pass3 body nenv mut)))]
    
    [(LetrecValues binds body)
     (let ([lhs : (SymDict Bool) (list-of-syms)])
       (let ([nenv : (SymDict Sym) (letrecvalues-extend-env binds lhs env)])
         (LetrecValues binds (pass3 body nenv mut))))]

    [(If cond then else)
     (If (pass3 cond env mut)
         (pass3 then env mut)
         (pass3 else env mut))]

    [(Begin exprs)
     (Begin (listexpr-pass3 exprs env mut))]

    [(Begin0 e1 exprs)
     (Begin0 (pass3 e1 env mut) (listexpr-pass3 exprs env mut))]

    [(App e1 exprs)  ;; (#%plain-app expr ...+)
     (App (pass3 e1 env mut) (listexpr-pass3 exprs env mut))]

    [(SetBang s e)
     (SetBang s (pass3 e env mut))]

    [(WithContinuationMark e1 e2 e3)
     (WithContinuationMark (pass3 e1 env mut)
                           (pass3 e2 env mut)
                           (pass3 e3 env mut))]))

(define (list-of-syms [lv : LVBIND]) : (SymDict Bool)
  (case lv
    [(CONSLVBIND syms e lv)
     (insert-syms syms (list-of-syms lv))]
    [(NULLLVBIND)
     (empty-dict)]))

(define (insert-syms [ls : ListSym] [syms : (SymDict Bool)]) : (SymDict Bool)
  (case ls
    [(CONSSYM s ls)
     (insert-syms ls (insert syms s #t))]
    [(NULLSYM)
     syms]))

(define (letrecvalues-extend-env [lv : LVBIND] [lhs : (SymDict Bool)] [env : (SymDict Sym)]) : (SymDict Sym)
  (case lv
    [(CONSLVBIND syms e lv)
     (letrecvalues-extend-env lv lhs (case e
                                       [(VARREF s)
                                        (if (has-key? env s)
                                            env
                                            (listsym-extend-env syms s env))]
                                       [_ env]))]
    [(NULLLVBIND)
     env]))

(define (letvalues-extend-env [lv : LVBIND] [env : (SymDict Sym)]) : (SymDict Sym)
  (case lv
    [(CONSLVBIND syms e lv)
     (letvalues-extend-env lv (case e
                                [(VARREF s)
                                 (listsym-extend-env syms s env)]
                                [_ env]))]
    [(NULLLVBIND)
     env]))

(define (listsym-extend-env [ls : ListSym] [sym : Sym] [env : (SymDict Sym)]) : (SymDict Sym)
  (case ls
    [(CONSSYM s ls)
     (listsym-extend-env ls sym (insert env s sym))]
    [(NULLSYM)
     env]))

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

;; gensym for a list of syms and insert mappings into environment
(define (rename-syms [ls : ListSym] [env : (SymDict Sym)]) : (SymDict Sym)
  (case ls
    [(CONSSYM s ls)
     (rename-syms ls (insert env s (gensym)))]
    [(NULLSYM)
     env]

(define (lvbind-pass2 [ls : LVBIND] [mut : (SymDict Bool)]) : (SymDict Bool)
  (case ls
    [(CONSLVBIND syms e ls)
     (lvbind-pass2 ls (pass2 e mut))]
    [(NULLLVBIND)
     mut]))

;; gensym let bindings and extend env with new mappings. 
(define (lvbind-rename [ls : LVBIND] [env : (SymDict Sym)]) : (SymDict Sym)
  (case ls
    [(CONSLVBIND syms e ls)
     (lvbind-rename ls (rename-syms syms env))]
    [(NULLLVBIND)
     env]))

;; 
(define (lvbind-update-lhs [ls : LVBIND] [env : (SymDict Sym)]) : LVBIND
  (case ls
    [(CONSLVBIND syms e ls)
     (CONSLVBIND (lookup-syms syms env)
                 e ;; what about here?
		 (lvbind-update-lhs ls env))]
    [(NULLLVBIND)
     ls]))

(define (lvbind-update-rhs [ls : LVBIND] [env : (SymDict Sym)]) : LVBIND
  (case ls
    [(CONSLVBIND syms e ls)
     (CONSLVBIND syms (pass1 e env) (lvbind-update-rhs ls env))]
    [(NULLLVBIND)
     ls]))

