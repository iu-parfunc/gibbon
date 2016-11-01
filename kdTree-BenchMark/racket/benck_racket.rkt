#lang s-exp "./treelang.rkt"


(data TreeIn
      [Leaf_In Float Float Int ]
      [Node_In Int Float Float Float Float Float TreeIn TreeIn ])

(data TreeOut
      [Leaf_Out Float Float Int ]
      [Node_Out Int Float TreeOut TreeOut ])

(data PointList
      [EndList]
      [Member Float Float Member ])


  


#| not yet done |#
(define (pointCorrelation [tr : TreeIn] [px : Float][py : Float] [rad : Float]) : TreeOut
  (case tr
    [(Leaf_In x y c ) ( Leaf_Out x y (if (positive?  (- rad (sqrt ( + (* ( - px x ) (- px  x)  ) (* ( - py y ) (- py  y)  ))   )))
                                       (+ c 1)
                                       ( c)
                                       ))]
  )
 )


#|

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

(let ([tr0 : Tree (build-tree 2)])
    (time (add1-tree tr0)))|#