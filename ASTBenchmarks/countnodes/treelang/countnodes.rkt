#lang s-exp "../../../Gibbon/treelang.rkt"

(require "../../grammar_racket.sexp")
(provide countnodes)

;; copied exactly


(define (countnodes [e0 : Toplvl]) : Int
  (top e0))

(define (loopTopLvl [ls : ListTopLvl]) : Int
  (case ls
    [(CONSTOPLVL tl ls)
     (+ (top tl) (loopTopLvl ls))]
    [(NULLTOPLVL)
     0]))

(define (loopExpr [ls : ListExpr]) : Int
  (case ls
    [(CONSEXPR e ls)
     (+ (expr e) (loopExpr ls))]
    [(NULLEXPR)
     0]))

(define (loopLambdaCase [ls : LAMBDACASE]) : Int
  (case ls
    [(CONSLAMBDACASE f le ls)
     (+ 1 (+ (formals f) (+ (loopExpr le) (loopLambdaCase ls))))]
    [(NULLLAMBDACASE)
     0]))

(define (loopLVBIND [ls : LVBIND]) : Int
  (case ls
    [(CONSLVBIND syms e ls)
     (+ 1 (+ (expr e) (loopLVBIND ls)))]
    [(NULLLVBIND)
     0]))

(define (top [e : Toplvl]) : Int
  (case e
    [(DefineValues ls e)
     (+ 1 (expr e))]
    [(DefineSyntaxes ls e)
     (+ 1 (expr e))]
    [(BeginTop ls)
     (+ 1 (loopTopLvl ls))]
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
     (+ 1 (+ (formals f) (loopExpr lse)))]
    [(CaseLambda cases)
     (+ 1 (loopLambdaCase cases))]
    [(LetValues binds body)
     (+ 1 (+ (loopLVBIND binds)
     	     (loopExpr body)))]
    [(LetrecValues binds body)
     (+ 1 (+ (loopLVBIND binds)
     	     (loopExpr body)))]
    [(If cond then else)
     (+ 1 (+ (expr cond) (+ (expr then) (expr else))))]
    [(Begin exprs)
     (+ 1 (loopExpr exprs))]
    [(Begin0 e1 exprs)
     (+ 1 (+ (expr e1) (loopExpr exprs)))]
    [(App exprs)  ;; (#%plain-app expr ...+)
     (+ 1 (loopExpr exprs))]
    [(SetBang s e)
     (+ 1 (expr e))]
    [(WithContinuationMark e1 e2 e3)
     (+ 1 (+ (expr e1) (+ (expr e2) (expr e3))))]))


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




