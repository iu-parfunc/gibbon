#lang s-exp "../TreeLang/treelang.rkt"

;; Manually flattened and uniquified:

(data Tree
      [Leaf Int]
      [Node Tree Tree])

(define (build-tree [n : Int]) : Tree
  (if (eq? n 0)
      (Leaf 100)
      (let ([min1  : Int (- n 1)])
      (let ([l : Tree (build-tree min1)]
            [r : Tree (build-tree min1)])
        (Node l r)))))

(define (add1-tree [tr : Tree]) : Tree
  (case tr
    [(Leaf n) (let ([nplus : Int (+ 1 n)])
                (Leaf nplus))]
    [(Node x y)
     (let ([x2 : Tree (add1-tree x)]
           [y2 : Tree (add1-tree y)])
       (Node x2 y2))]))

(let ([tr0 : Tree (build-tree 20)])
    (time (add1-tree tr0)))
