#lang s-exp "../treelang.rkt"

(data Foo (MkFoo Int))

(define (foo [x : Foo]) : Int
  (case x
    [(MkFoo n) n]))

500
