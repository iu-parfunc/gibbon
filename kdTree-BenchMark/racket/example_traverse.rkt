#lang s-exp "../../Gibbon/gibbon.rkt"

(provide (all-defined-out))

(data Test
      [Foo Int]
      [Bar Tree Tree])

(define (traverse [tr : Test]) : Test
  tr ;; identity function for now...
  )
