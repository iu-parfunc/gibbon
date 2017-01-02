#lang gibbon

(data Nat [Zero] [Suc Nat])

(data Foo [A Int] [B Int Int])

(case (B 2 4)
  [(A x)   x] 
  [(B x y) x])

