#lang gibbon

(data Foo (A) 
          (B Int Foo))

(define (foo [ev : Foo]) : Int
  (case ev
    [(A)  100]
    [(B x y) 300]))
