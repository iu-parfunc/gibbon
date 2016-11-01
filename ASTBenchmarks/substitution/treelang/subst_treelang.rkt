#lang s-exp "../../../TreeLang/treelang.rkt"

(require "../../grammar_racket.sexp")
(provide subst)

;; copied exactly


(define (subst [old : Sym] [new : Sym] [e0 : Toplvl]) : Toplvl
  (top old new e0))

#|
(data Toplvl
      [DefineValues   (Listof Sym) Expr]
      [DefineSyntaxes (Listof Sym) Expr]
      [Expression Expr])
|#
(define (top [old : Sym] [new : Sym] [e : Toplvl]) : Toplvl
  (case e
    [(DefineValues ls e)
     (DefineValues ls (expr old new e))]
    [(DefineSyntaxes ls e)
     (DefineSyntaxes ls (expr old new e))]
    [(BeginTop ls)
     (BeginTop (for/list ([t : Toplvl ls])
                 (top old new t)))]
    [(Expression e)
     (Expression (expr old new e))]))

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
                         (for/list ([e : Expr lse])
                           (expr old new e))))]
    [(CaseLambda cases)
     (CaseLambda (for/list ([c : LAMBDACASE cases])
                   (case c
                     [(MKLAMBDACASE formals exprs)
                      (if (bound-in? old formals)
                          c
                          (MKLAMBDACASE formals (for/list ([e : Expr exprs])
                                                  (expr old new e))))])))]
    [(LetValues binds body)
     (LetValues (for/list ([b : LVBIND binds])
                  (case b
                    [(MKLVBIND syms e)
                     (if (helper old syms)
                         b
                         (MKLVBIND syms (expr old new e)))]))

                (for/list ([e : Expr body])
                  (expr old new e)))]
    [(LetrecValues binds body)
     (LetrecValues (for/list ([b : LVBIND binds])
                     (case b
                       [(MKLVBIND syms e)
                        (if (helper old syms)
                            b
                            (MKLVBIND syms (expr old new e)))]))
                   
                   (for/list ([e : Expr body])
                     (expr old new e)))]
    [(If cond then else)
     (If (expr old new cond) (expr old new then) (expr old new else))]
    [(Begin exprs)
     (Begin (for/list ([e : Expr exprs])
              (expr old new e)))]
    [(Begin0 e1 exprs)
     (Begin0 (expr old new e1) (for/list ([e : Expr exprs])
                                 (expr old new e)))]
    [(App exprs)  ;; (#%plain-app expr ...+)
     (App (for/list ([e : Expr exprs])
            (expr old new e)))]
    [(SetBang s e)
     (SetBang s (expr old new e))]
    [(WithContinuationMark e1 e2 e3)
     (WithContinuationMark (expr old new e1) (expr old new e2) (expr old new e3))]))

(define (bound-in? [sym : Sym] [formals : Formals]) : Bool
  (case formals
    [(F1 syms)
     (helper sym syms)]
    [(F2 syms s)
     (if (eq? sym s)
         #t
         (helper sym syms))]
    [(F3 s)
     (eq? sym s)]))

(define (helper [sym : Sym] [ls : (Listof Sym)]) : Bool
  (if (empty? ls)
      #f
      (if (eq? (car ls) sym)
          #t
          (helper sym (cdr ls)))))




