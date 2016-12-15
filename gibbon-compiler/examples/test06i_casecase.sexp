#lang s-exp "../../gibbon/main.rkt"

(data Nat [Zero] [Suc Nat])

(case 
    (case (Suc (Zero))
      [(Zero)  (Zero)]
      [(Suc xx) (Zero)])
  [(Zero) 99]
  [(Suc yy) 101])

