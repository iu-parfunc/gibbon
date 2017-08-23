#lang gibbon

(require "R0.gib")

(define (interp [e : Exp]) : Int
  (case e
    [(LitE n) n]
    [(ReadE) 999] ;; Hack, fixme.
    [(NegE e) (- 0 (interp e))]
    [(AddE e1 e2) (+ (interp e1) (interp e2))]))

(interp (AddE (NegE (LitE 3)) (ReadE)))

