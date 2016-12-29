#lang gibbon

(data Foo (A) 
          (B Int Foo))

(define (foo [e : Foo]) : Int
  (case e
    [(A)  100]
    [(B x y) 300]))
