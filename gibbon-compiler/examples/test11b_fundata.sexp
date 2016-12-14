#lang s-exp "../gibbon.rkt"

(data Foo (MkFoo Int))

(define (foo [x : Foo]) : Int
  (case x
    [(MkFoo n) n]))

(foo (MkFoo 501))
