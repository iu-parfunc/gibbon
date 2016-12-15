#lang s-exp "../../gibbon/main.rkt"

(data Nat [Zero] [Suc Nat])

(let ([x : Nat (case (Suc (Zero))
                 [(Zero)  (Zero)]
                 [(Suc _) (Zero)])])
  1000)
