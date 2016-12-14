#lang s-exp "../gibbon.rkt"

(data Nat [Zero] [Suc Nat])

(define (build ) : Nat
  (Suc (Zero)))

(case (build)
  [(Zero) 0]
  [(Suc _n) 1])
