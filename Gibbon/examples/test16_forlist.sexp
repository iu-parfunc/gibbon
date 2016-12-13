#lang s-exp "../gibbon.rkt"

(data Foo (MkFoo (Listof Int)))

(define (add1 [x : Foo]) : Foo
  (case x
    [(MkFoo ls)
     (MkFoo (for/list ([ x : Int ls]) (+ 1 x)))]))
