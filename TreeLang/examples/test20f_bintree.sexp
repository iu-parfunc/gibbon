#lang s-exp "../treelang.rkt"

(data Tree
      [Leaf Int]
      [Node Tree Tree])


(define (add1-tree [tr : Tree]) : Tree
  (case tr
    [(Leaf n) (Leaf (+ 1 n))]
    [(Node x y) (Node (add1-tree x)
                      (add1-tree y))]))

;; Here we time a fold over the tree:
(let ((tr0 : Tree (time (add1-tree (Leaf 33)))))
  987)
