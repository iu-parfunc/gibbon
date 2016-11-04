#lang s-exp "../../../TreeLang/treelang.rkt"

(require "../../grammar_racket.sexp")
(provide countnodes)

;; copied exactly


(define (countnodes [e0 : Toplvl]) : Int
  (top e0))

#|
(data Toplvl
      [DefineValues   (Listof Sym) Expr]
      [DefineSyntaxes (Listof Sym) Expr]
      [Expression Expr])
|#
(define (top [e : Toplvl]) : Int
  (case e
    [(DefineValues ls e)
     (+ 1 (expr e))]
    [(DefineSyntaxes ls e)
     (+ 1 (expr e))]
    [(BeginTop ls)
     (+ 1 (for/fold : Int ([sum : Int 0])
                          ([t : Toplvl ls])
            (+ (top t) sum)))]
    [(Expression e)
     (+ 1 (expr e))]))

(define (expr [e : Expr]) : Int
  (case e
    ;; Variable references:
    [(VARREF s)
     1]
    [(Top s)
     1]
    [(VariableReference s)   ; #%variable-reference
     1] 
    [(VariableReferenceTop s)   ; #%variable-reference (#%top . id)
     1]

    ;; Leaf forms:
    [(VariableReferenceNull)     ; (#%variable-reference)
     1]
    [(Quote d)
     (+ 1 (datum d))]
    [(QuoteSyntax d)
     (+ 1 (datum d))]
    [(QuoteSyntaxLocal d)
     (+ 1 (datum d))] ;; (quote-syntax datum #:local)

    ;; Binding forms:
    [(Lambda f lse)
     (+ 1 (+ (formals f) (for/fold : Int ([sum : Int 0])
                                      ([e : Expr lse])
                        (+ (expr e) sum))))]
    [(CaseLambda cases)
     (+ 1 (for/fold : Int ([sum : Int 0])
                          ([lc : LAMBDACASE cases])
            (+ (lambdacase lc) sum)))]
    [(LetValues binds body)
     (+ 1 (+ (for/fold : Int ([sum : Int 0])
               ([lvb : LVBIND binds])
               (+ (lvbind lvb) sum))
             (for/fold : Int ([sum : Int 0])
               ([e : Expr body])
               (+ (expr e) sum))))]
    [(LetrecValues binds body)
     (+ 1 (+ (for/fold : Int ([sum : Int 0])
               ([lvb : LVBIND binds])
               (+ (lvbind lvb) sum))
             (for/fold : Int ([sum : Int 0])
               ([e : Expr body])
               (+ (expr e) sum))))]
    [(If cond then else)
     (+ 1 (+ (expr cond) (+ (expr then) (expr else))))]
    [(Begin exprs)
     (+ 1 (for/fold : Int ([sum : Int 0])
                          ([e : Expr exprs])
            (+ (expr e) sum)))]
    [(Begin0 e1 exprs)
     (+ 1 (+ (expr e1) (for/fold : Int ([sum : Int 0])
                         ([e : Expr exprs])
                         (+ (expr e) sum))))]
    [(App exprs)  ;; (#%plain-app expr ...+)
     (+ 1 (for/fold : Int ([sum : Int 0])
                          ([e : Expr exprs])
            (+ (expr e) sum)))]
    [(SetBang s e)
     (+ 1 (expr e))]
    [(WithContinuationMark e1 e2 e3)
     (+ 1 (+ (expr e1) (+ (expr e2) (expr e3))))]))

(define (lvbind [b : LVBIND]) : Int
  (case b
    [(MKLVBIND syms e)
     (+ 1 (expr e))]))

(define (lambdacase [lc : LAMBDACASE]) : Int
  (case lc
    [(MKLAMBDACASE f exprs)
     (+ 1 (+ (formals f) (for/fold : Int ([sum : Int 0])
                           ([e : Expr exprs])
                           (+ (expr e) sum))))]))

(define (datum [d : Datum]) : Int
  (case d
    [(INTLIT i)
     1]))

(define (formals [f : Formals]) : Int
  (case f
    [(F1 ls)
     1]
    [(F2 ls s)
     1]
    [(F3 s)
     1]))




