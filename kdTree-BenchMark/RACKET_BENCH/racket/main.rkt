#lang racket
  
(require "traversal.rkt")
(require racket/cmdline)
(require "./treelang.rkt")

;;(write (Leaf 1.1 1.1 2))
;;(define args (current-command-line-arguments))
;;(write  (vector-ref 0 args) )


(define inputfiles
  (vector  "../inputs/in_1000.txt" "../inputs/in_10000.txt" "../inputs/in_100000.txt"
          "../inputs/in_1000000.txt" "../inputs/in_10000000.txt" "../inputs/in_100000000.txt") )


(define (lesX a b) ( < (car a) (car b) ) )
(define (lesY a b) ( < (cdr a) (cdr b) )  )

(define (buildTree strtIndex endIndex  depth pLst)
  (let ( [size ( + ( - endIndex  strtIndex) 1) ] [mid  (quotient  (+ endIndex strtIndex ) 2 ) ] )

    ;;do always sort mutable vector
   (if (even? depth) (vector-sort! pLst lesX strtIndex (+ endIndex 1) ) (vector-sort! pLst lesY strtIndex  (+ endIndex 1)) )


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

;;( define inputTree  (buildTree 0 (- (vector-length pointList) 1) 0 pointList) )
(newline)


;;(newline)
;;(write (pointCorrelation  inputTree 0.0 0.0 20.0 ))

(newline)
(write "running ...... ")
(newline)
(define timestrt 0.0)
(define timeend 0.0)
(define timesum 0.0)
( for    ( [j (in-range 7)] )
          (newline) (write "Reading file:" )(write (vector-ref inputfiles j))
           (let ([ pointList (read (open-input-file (vector-ref inputfiles j) ) )])
                (let ([inputTree  (buildTree 0 (- (vector-length pointList) 1) 0 pointList)])
                   (newline)(write "running tree out") (set! timestrt (current-inexact-milliseconds))
                   (for   ([x (in-range 10)])
                          (for ( [p (in-range 10)] )
                             (pointCorrelation  inputTree (car (vector-ref pointList p)) (cdr (vector-ref pointList p)) 0.7 )))
                           
                  (set! timeend (current-inexact-milliseconds))
                   (write "avg time :") (write (fl/ (fl- timeend timestrt) 10.0))
                  
    
                   (newline)(write "running int out")(set! timestrt (current-inexact-milliseconds))
                     (for ([x (in-range 10)])
                          (for ( [p (in-range 10)] )
                          (pointCorrelation_v2  inputTree (car (vector-ref pointList p)) (cdr (vector-ref pointList p)) 0.7 )
                           )
                     )
                        (set! timeend (current-inexact-milliseconds))
                   (write "avg time :") (write (fl/ (fl- timeend timestrt) 10.0))(newline)
               

                  
               )
           
             )
   )


   



;;(for ( [p pointList] )  (write   (pointCorrelation_v2  inputTree (car p) (cdr p) 0.7 ) ) ( newline))


