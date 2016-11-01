#lang s-exp "../../TreeLang/treelang.rkt"

(provide (all-defined-out))
;; Or: (provide Foo Foo? Bar Bar?)

(data Tree
      [Foo Int]
      [Bar Tree Tree])


(define (traverse [tr : Tree]) : Tree
  tr ;; identity function for now...
  )
