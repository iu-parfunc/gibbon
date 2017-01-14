#lang gibbon


(data Toplvl
      [DefineValues   ListSym Expr]
      [DefineSyntaxes ListSym Expr]
      [BeginTop ListToplvl]
      [Expression Expr])

(data Expr
      (VARREF Sym)              ;; Tag 0
      (CaseLambda LAMBDACASE)
      )

(data LVBIND
      (CONSLVBIND ListSym Expr LVBIND)
      (NULLLVBIND ))

(data LAMBDACASE
      (CONSLAMBDACASE Formals ListExpr LAMBDACASE)  ;; (formals expr ...+) 
      (NULLLAMBDACASE ))

(data Formals
      [F1 ListSym]        ;; Tag 0
      [F2 ListSym Sym]    ;; Tag 1
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
    [(DefineValues ls e)    (DefineValues (sym-ls ls) (expr e))]
    [(DefineSyntaxes ls e)  (DefineSyntaxes (sym-ls ls) (expr e))]
    [(BeginTop ls)          (BeginTop (top-ls ls))]
    [(Expression e)         (Expression (expr e))]))

(define (top-ls [es : ListToplvl]) : ListToplvl
  (case es
    [(CONSTOPLVL e es)  (CONSTOPLVL (treewalk e) (top-ls es))]
    [(NULLTOPLVL)       (NULLTOPLVL)]))

(define (expr-ls [es : ListExpr]) : ListExpr
  (case es
    [(CONSEXPR e es) (CONSEXPR (expr e) (expr-ls es))]
    [(NULLEXPR) (NULLEXPR)]))

(define (walk-lvbind [lv : LVBIND]) : LVBIND
  (case lv
    [(NULLLVBIND) (NULLLVBIND)]
    [(CONSLVBIND syms e rest)
     (CONSLVBIND (sym-ls syms) (expr e) (walk-lvbind rest))]))

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
    [(F1 syms)   (F1 (sym-ls syms))]
    [(F2 syms s) (F2 (sym-ls syms) s)]
    [(F3 s)      (F3 s)]))

(define (expr [e : Expr]) : Expr
  (case e
    [(VARREF s)               (VARREF s)]
    [(CaseLambda cases)        (CaseLambda (walk-lambdacase cases))]))

;; This passes in --pointer but gives an type error in --packed:
(let ((x : Toplvl (treewalk (Expression (CaseLambda (NULLLAMBDACASE)))))) 0)
