#lang racket
  
(require "traversal.rkt")
(require "treelang.rkt")

;;(write (Leaf 1.1 1.1 2))



(define pointList (vector (cons 10.0 10.0) (cons 100.0 100.0) (cons 1.0 2.0)))


(define (lesX a b) ( < (car a) (car b) ) )
(define (lesY a b) ( < (cdr a) (cdr b) ) )

(define (buildTree strtIndex endIndex  depth pLst)
  (let ( [size ( + ( - endIndex  strtIndex) 1) ] [mid  (quotient  (+ endIndex strtIndex ) 2 ) ] )

    ;;do always sort mutable vector 
    (if (even? depth) (vector-sort! pLst lesX strtIndex endIndex) (vector-sort! pLst lesY strtIndex endIndex) )
    (if (= size 1 )
        ;;then
        ( Leaf (car (vector-ref pLst mid))  (cdr  (vector-ref pLst mid)) 0 )
        ;;else
        ( let   ([splitAccess   (if (even? depth) 1 0 )  ]
                 [splitLocation (if (even? depth) (car (vector-ref pLst mid)) (cdr (vector-ref pLst mid) ))]
                 [rightChild (buildTree (+ mid 1 ) endIndex  (+ depth 1) pLst)  ]
                 [leftChild  (buildTree strtIndex mid  (+ depth 1 ) pLst)       ]
                 )
                (let ( [ minxRight (match rightChild [(Leaf x y c) x]  [(Node _ _ minX _ _ _ _ _ ) minX])]
                       [ maxxRight (match rightChild [(Leaf x y c) x]  [(Node _ _ _ maxX _ _ _ _ ) maxX])]
                       [ minyRight (match rightChild [(Leaf x y c) y]  [(Node _ _ _ _ minY _ _ _ ) minY])]
                       [ maxyRight (match rightChild [(Leaf x y c) y]  [(Node _ _ _ _ _ maxY _ _ ) maxY])]
                       [ minxLeft (match leftChild [(Leaf x y c) x]  [(Node _ _ minX _ _ _ _ _ ) minX])]
                       [ maxxLeft (match leftChild [(Leaf x y c) x]  [(Node _ _ _ maxX _ _ _ _ ) maxX])]
                       [ minyLeft (match leftChild [(Leaf x y c) y]  [(Node _ _ _ _ minY _ _ _ ) minY])]
                       [ maxyLeft (match leftChild [(Leaf x y c) y]  [(Node _ _ _ _ _ maxY _ _ ) maxY])]
                      )
                  
                  (  Node  splitAccess splitLocation ( min minxRight minxLeft) ( max maxxRight maxxLeft) ( min minyRight minyLeft) ( max maxyRight maxyLeft) rightChild leftChild )  
        )
        )
    )
  )
  )

   

;;(  Leaf (car (vector-ref lst 0))  (cdr  (vector-ref lst 0)) 0)   )

( define inputTree  (buildTree 0 (- (vector-length pointList) 1) 0 pointList) )
(newline)
(write inputTree)

(newline)
(write (pointCorrelation  inputTree 0.0 0.0 20.0 ))
(newline)
(write (pointCorrelation_v2  inputTree 0.0 0.0 20.0 ))


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