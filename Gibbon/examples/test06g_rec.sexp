#lang s-exp "../treelang.rkt"

(data Nat [Zero] [Suc Nat])

(case (Suc (Zero))
  [(Zero) 1000]
  [(Suc _) 999])
