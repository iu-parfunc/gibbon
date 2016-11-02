#lang s-exp "../treelang.rkt"

(data Foo (MkFoo Int))

(define (eat [x : Foo]) : Int
  (case x
    [(MkFoo n) n]))

(eat (MkFoo 3))
