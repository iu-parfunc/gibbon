#lang s-exp "../gibbon.rkt"

(data Nat [Zero] [Suc Nat])

(case (Suc (Zero))
  [(Zero) 1000]
  [(Suc _) 999])
