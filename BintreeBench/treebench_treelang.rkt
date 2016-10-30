#lang s-exp "../TreeLang/treelang.rkt"

(data Tree
      [Leaf Int]
      [Node Tree Tree])

(define (go [root : Int] [n : Int]) : Tree
  (if (eq? n 0)
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

(let ([tr0 : Tree (build-tree 20)])
    (time (add1-tree tr0))
    
    ;; (for/list ([x '(1 2 3 4 5 6 7 8 9 10)])
    ;;     (let ([_ (time (add1-tree tr0))])
    ;;       0
    ;;       ))
    )

; (error "Hello world")
