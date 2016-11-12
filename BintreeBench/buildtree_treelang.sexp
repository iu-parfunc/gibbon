#lang s-exp "../TreeLang/treelang.rkt"

(data Tree
      [Leaf Int]
      [Node Tree Tree])

(define (build-tree [n : Int]) : Tree
  (if (= n 0)
      (Leaf 1)
      (let ([l : Tree (build-tree (- n 1))]
            [r : Tree (build-tree (- n 1))])
        (Node l r))))

;; If we don't time it, the compiler will drop it:
(let ((tr0 : Tree (iterate (build-tree (size-param)))))
  12345)
