#lang s-exp "../TreeLang/treelang.rkt"

(provide (all-defined-out))

(data Tree
      [Leaf Int]
      [Node Tree Tree])

(define (go [root : Int] [n : Int]) : Tree
  (if (= n 0)
      (Leaf root)
      (Node (go root (- n 1))
            (go (+ root 1) ; (expt 2 (sub1 n))
                (- n 1)))))

(define (build-tree [n : Int]) : Tree
  (go 1 n))

(define (add1-tree [tr : Tree]) : Tree
  (case tr
    [(Leaf n) (Leaf (+ 1 n))]
    [(Node x y) (Node (add1-tree x)
                      (add1-tree y))]))

(module+ main
  (let ([tr0 : Tree (build-tree (size-param))])
    (let ([tr1 : Tree (iterate (add1-tree tr0))])
      0    
      ))
  )


; (error "Hello world")
