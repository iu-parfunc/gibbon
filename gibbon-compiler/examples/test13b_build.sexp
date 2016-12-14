#lang s-exp "../gibbon.rkt"

(data Nat [Zero] [Suc Nat])

(define (build [n : Int]) : Nat
  (Suc (Zero)))

(case (build 99)
  [(Zero) 0]
  [(Suc _n) 1])

