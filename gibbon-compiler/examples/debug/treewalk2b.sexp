#lang gibbon

(data Toplvl
      [Expression Expr])

(data Expr
      (VARREF Sym)              ;; Tag 0
      (CaseLambda LAMBDACASE)
      )

(data LAMBDACASE
      (CONSLAMBDACASE Formals ListExpr LAMBDACASE)  ;; (formals expr ...+) 
      (NULLLAMBDACASE ))

(data Formals
      [F3 Sym])                ;; Tag 2

(data ListToplvl
      (CONSTOPLVL Toplvl ListToplvl)
      (NULLTOPLVL))

(data ListExpr
      (CONSEXPR Expr ListExpr)
      (NULLEXPR))

(data ListSym
      (CONSSYM Sym ListSym)
      (NULLSYM))

;; Identity function on ASTs:
(define (treewalk [e : Toplvl]) : Toplvl
  (case e
    [(Expression e)         (Expression (expr e))]))

(define (top-ls [es : ListToplvl]) : ListToplvl
  (case es
    [(CONSTOPLVL e es)  (CONSTOPLVL (treewalk e) (top-ls es))]
    [(NULLTOPLVL)       (NULLTOPLVL)]))

(define (expr-ls [es : ListExpr]) : ListExpr
  (case es
    [(CONSEXPR e es) (CONSEXPR (expr e) (expr-ls es))]
    [(NULLEXPR) (NULLEXPR)]))

(define (walk-lambdacase [lc : LAMBDACASE]) : LAMBDACASE
       (case lc
         [(NULLLAMBDACASE) lc]
         [(CONSLAMBDACASE formals exprs rest)
          (CONSLAMBDACASE (walk-formals formals) (expr-ls exprs) (walk-lambdacase rest))]))

(define (sym-ls [ls : ListSym]) : ListSym
  (case ls
    [(CONSSYM s ls) (CONSSYM s (sym-ls ls))]
    [(NULLSYM)      (NULLSYM)]))

(define (walk-formals [formals : Formals]) : Formals
  (case formals
    [(F3 s)      (F3 s)]))

(define (expr [e : Expr]) : Expr
  (case e
    [(VARREF s)               (VARREF s)]
    [(CaseLambda cases)        (CaseLambda (walk-lambdacase cases))]))


;; This stack-overflows after calling a long long chain of unpack_ListSym:
(treewalk (Expression (CaseLambda (NULLLAMBDACASE))))
