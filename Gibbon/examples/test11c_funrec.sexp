#lang s-exp "../treelang.rkt"

(data Nat [Zero] [Suc Nat])

(define (trav [x : Nat]) : Int
  (case x
    [(Zero) 0]
    [(Suc n) (+ 1 (trav n))]))

(trav (Zero))
