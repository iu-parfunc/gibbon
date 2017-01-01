#lang gibbon

(data Tree
      [Leaf Int]
      [Node Tree Tree])

;; Compile the basic map add1 over leaves of a tree.
(define (add1-tree [tr : Tree]) : Tree
  (case tr
    [(Leaf n) (Leaf (+ 1 n))]
    [(Node x y) (Node (add1-tree x)
                      (add1-tree y))]))

;; Don't run anything yet, just compile the function for this test.
386
