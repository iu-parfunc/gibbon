#lang s-exp "../../gibbon/main.rkt"

(data Nat [Zero] [Suc Nat])

(case (Suc (Zero))
  [(Zero) 1000]
  [(Suc _) 999])
