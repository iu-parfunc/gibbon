#lang s-exp "../treelang.rkt"

;; Data type of unknown size.
(data Nat [Zero] [Suc Nat])

;; A field stored after the unknown
(data Foo [MkFoo Nat Int])

(case (MkFoo (Suc (Zero)) 71)
  [(MkFoo x y) y])
