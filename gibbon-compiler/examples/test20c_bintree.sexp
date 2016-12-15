#lang s-exp "../../gibbon/main.rkt"

(data Tree
      [Leaf Int]
      [Node Tree Tree])

(define (build-tree [n : Int]) : Tree
  (if (= n 0)
      (Leaf 1)
      (let ([l : Tree (build-tree (- n 1))]
            [r : Tree (build-tree (- n 1))])
        (Node l r))))

(define (sum-tree [tr : Tree]) : Int
  (case tr
    [(Leaf n) n]
    [(Node x y) (+ (sum-tree x) (sum-tree y))]))

;; Here we time a fold over the tree:
(let ((tr0 : Tree (Leaf 99)))
  (sum-tree tr0))
