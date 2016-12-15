#lang s-exp "../../gibbon/main.rkt"

(data Nat [Zero] [Suc Nat])

(let ([_ : Nat (Suc (Suc (Zero)))])
  1000)
