#lang s-exp "./treelang.rkt"

(provide Foo Foo? Bar Bar?)

(data Tree
      [Foo Int]
      [Bar Tree Tree])


(define (traverse [tr : Tree]) : Tree
  tr ;; identity function for now...
  )
