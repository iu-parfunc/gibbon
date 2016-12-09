#lang s-exp "../treelang.rkt"

(data Nat [Zero] [Suc Nat])

(let ([_ : Nat (Zero)])
  1000)
