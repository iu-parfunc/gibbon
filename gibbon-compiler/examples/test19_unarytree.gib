#lang gibbon

;; A really boring "tree"
(data Tree [Leaf Int] [Node Tree])

;; Compile the basic map add1 over the leaf.
(define (add1-tree [tr : Tree]) : Tree
  (case tr
    [(Leaf n) (Leaf (+ 1 n))]
    [(Node x) (Node (add1-tree x))]))

;; Don't run anything yet, just compile the function for this test.
386
