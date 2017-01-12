#lang gibbon

;; Minature version of countnodes for debugging.

;; -------------
;; From: (require "../../ASTBenchmarks/grammar_racket.gib")

(data Toplvl
      ;; [DefineValues   ListSym Expr]
      ;; [DefineSyntaxes ListSym Expr]
      ;; [BeginTop ListToplvl]
      ;; [Expression Expr]
      [Expression Int]
      )


;; -------------
;; From: (require "../../ASTBenchmarks/countnodes/treelang/countnodes.rkt")

(define (countnodes [e0 : Toplvl]) : Int
  (top e0))

(define tag : Int 1)

(define (top [e : Toplvl]) : Int
  (case e
    ;; [(DefineValues ls e)
    ;;  (+ tag (+ (loopSyms ls) (expr e)))]
    ;; [(DefineSyntaxes ls e)
    ;;  (+ tag (+ (loopSyms ls) (expr e)))]
    ;; [(BeginTop ls)
    ;;  (+ tag (loopTopLvl ls))]
    [(Expression e)
     (+ tag (expr e))]
    ))

(define (expr [_ : Int]) : Int ;; stub
  1) 

; ----------------------------------------

(top (Expression 33))

