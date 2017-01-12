#lang gibbon

(require "../../grammar_racket.sexp")
(provide countnodes
         ;; Provide other functions for testing purposes:
         datum formals expr top)

;; copied exactly

;; The main entrypoint:
(define (countnodes [e0 : Toplvl]) : Int
  (top e0))

;; For list encodings, do we count the NULL at the end?
;; (It's a regular tag, so probably yes.)
(define nullcost : Int 1)  
;; Likewise, the cost of a cons.  We can simulate direct list support
;; by tweaking conscost and nullcost.
(define conscost : Int 1)

;; For Sym and Int leaves, what do they cost?
;; If we set this to "8" we should be able to count byte-size exactly.
(define scalarcost : Int 0)

;; How do we count a tag?  We can set this to zero to just count the
;; "payload" or we can set it to 8 to model fat tags, or even 16 to
;; model fat tags PLUS a pointer to the tagged object.
(define tag : Int 1)


(define (loopTopLvl [ls : ListToplvl]) : Int
  (case ls
    [(CONSTOPLVL tl ls)
     (+ conscost (+ (top tl) (loopTopLvl ls)))]
    [(NULLTOPLVL)
     nullcost]))

(define (loopExpr [ls : ListExpr]) : Int
  (case ls
    [(CONSEXPR e ls)
     (+ conscost (+ (expr e) (loopExpr ls)))]
    [(NULLEXPR)
     nullcost]))

(define (loopLambdaCase [ls : LAMBDACASE]) : Int
  (case ls
    [(CONSLAMBDACASE f le ls)
     (+ conscost (+ (formals f) (+ (loopExpr le) (loopLambdaCase ls))))]
    [(NULLLAMBDACASE)
     nullcost]))

(define (loopLVBIND [ls : LVBIND]) : Int
  (case ls
    [(CONSLVBIND syms e ls)
     (+ conscost (+ (loopSyms syms) 
                    (+ (expr e) (loopLVBIND ls))))]
    [(NULLLVBIND)
     nullcost]))

(define (top [e : Toplvl]) : Int
  (case e
    [(DefineValues ls e)
     (+ tag (+ (loopSyms ls) (expr e)))]
    [(DefineSyntaxes ls e)
     (+ tag (+ (loopSyms ls) (expr e)))]
    [(BeginTop ls)
     (+ tag (loopTopLvl ls))]
    [(Expression e)
     (+ tag (expr e))]))

(define (expr [e : Expr]) : Int
  (case e
    ;; Variable references:
    [(VARREF s)
     (+ tag scalarcost)]
    [(Top s)
     (+ tag scalarcost)]
    [(VariableReference s)   ; #%variable-reference
     (+ tag scalarcost)]
    [(VariableReferenceTop s)   ; #%variable-reference (#%top . id)
     (+ tag scalarcost)]

    ;; Leaf forms:
    [(VariableReferenceNull)     ; (#%variable-reference)
     tag]
    [(Quote d)
     (+ tag (datum d))]
    [(QuoteSyntax d)
     (+ tag (datum d))]
    [(QuoteSyntaxLocal d)
     (+ tag (datum d))] ;; (quote-syntax datum #:local)

    ;; Binding forms:
    [(Lambda f lse)
     (+ tag (+ (formals f) (loopExpr lse)))]
    [(CaseLambda cases)
     (+ tag (loopLambdaCase cases))]
    [(LetValues binds body)
     (+ tag (+ (loopLVBIND binds)
               (loopExpr body)))]
    [(LetrecValues binds body)
     (+ tag (+ (loopLVBIND binds)
               (loopExpr body)))]
    [(If cond then else)
     (+ tag (+ (expr cond) (+ (expr then) (expr else))))]
    [(Begin exprs)
     (+ tag (loopExpr exprs))]
    [(Begin0 e1 exprs)
     (+ tag (+ (expr e1) (loopExpr exprs)))]
    [(App e1 es)  ;; (#%plain-app expr ...+)
     (+ tag (+ (expr e1) (loopExpr es)))]
    [(SetBang s e)
     (+ tag (+ scalarcost (expr e)))]
    [(WithContinuationMark e1 e2 e3)
     (+ tag (+ (expr e1) (+ (expr e2) (expr e3))))]))


(define (datum [d : Datum]) : Int
  (case d
    [(INTLIT i)
     (+ tag scalarcost)]))

(define (loopSyms [ls : ListSym]) : Int
  (case ls
    [(CONSSYM s ls)
     (+ conscost (+ scalarcost (loopSyms ls)))]
    ; [(NULLTOPLVL) nullcost] ;; This type error is not caughty by #lang gibbon / typed racket
    [(NULLSYM) nullcost]
    ))

(define (formals [f : Formals]) : Int
  (case f
    [(F1 ls)
     (+ tag (loopSyms ls))]
    [(F2 ls s)
     (+ tag (+ (loopSyms ls) scalarcost))]
    [(F3 s)
     (+ tag scalarcost)]))
