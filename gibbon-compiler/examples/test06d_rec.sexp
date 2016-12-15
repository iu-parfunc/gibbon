#lang s-exp "../../gibbon/main.rkt"

(data Nat [Zero] [Suc Nat])

(let ([_ : Nat (Zero)])
  1000)
