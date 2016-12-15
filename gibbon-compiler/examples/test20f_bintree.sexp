#lang s-exp "../../gibbon/main.rkt"

(data Tree
      [Leaf Int]
      [Node Tree Tree])


(define (add1-tree [tr : Tree]) : Tree
  (case tr
    [(Leaf n) (Leaf (+ 1 n))]
    [(Node x y) (Node (add1-tree x)
                      (add1-tree y))]))

386
