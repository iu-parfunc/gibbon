#lang racket
  
(require "traversal.rkt")
(require "treelang.rkt")

;;(write (Leaf 1.1 1.1 2))



(define pointList (vector (cons 1.0 1.0) (cons 1.0 2.0) (cons 2.0 3.0)))


(define (lesX a b) ( < (car a) (car b) ) )
(define (lesY a b) ( < (cdr a) (cdr b) ) )

(define (buildTree strtIndex endIndex  depth pLst)
  (let ( [size ( + ( - endIndex  strtIndex) 1) ] [mid  (quotient  (+ endIndex strtIndex ) 2 ) ] )
    ( 
     if (= size 1 )
        ( Leaf (car (vector-ref pLst mid))  (cdr  (vector-ref pLst mid)) 0 ) 
        (   let ([splitAccess   (if (even? depth) 1 0 )  ]
                 [splitLocation (if (even? depth) (car (vector-ref pLst mid)) (cdr (vector-ref pLst mid) ))] 
                 )
             (if (even? depth) (vector-sort! pLst lesX strtIndex endIndex) (vector-sort! pLst lesY strtIndex endIndex) )
             ( Node  splitAccess splitLocation 0.0 0.0 0.0 0.0 (buildTree (+ mid 1 ) endIndex  (+ depth 1) pLst) (buildTree strtIndex mid  (+ depth 1 ) pLst)) )  
        )
    )
  )
  
    

   

;;(  Leaf (car (vector-ref lst 0))  (cdr  (vector-ref lst 0)) 0)   )


(write (buildTree 0 2 0  pointList ))



 #|

;; build-tree can use the full Racket language.
;; In fact, this code is not type checked.
(define (build-tree sz)
  (Bar (Foo 3) (Foo 1)))

(define (bench sz iters)
  (define tr (build-tree sz))
  ;; Use the traversal written in treelang
  (time
   (for ((_ (in-range iters)))
     (traverse tr))))

(bench 20 10)
(display "Done benchmarking\n")


(define (build-tree [pList : PointList ]) : Tree
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