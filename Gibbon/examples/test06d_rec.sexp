#lang s-exp "../gibbon.rkt"

(data Nat [Zero] [Suc Nat])

(let ([_ : Nat (Zero)])
  1000)
