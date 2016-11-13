#lang s-exp "./treelang.rkt"

(provide (all-defined-out))
; (provide Leaf Leaf? Node Node?  Int pointCorrelation pointCorrelation_v2)


(data Tree
      [Leaf Float Float Int ]
      [Node Int Float Float Float Float Float Tree Tree ])



(data Point
      [mkpoint Float Float])


(define (canCorrelate [minX : Float] [maxX : Float]
                      [minY : Float] [maxY : Float]
                      [px : Float] [py : Float] [rad : Float]) : Bool
  (let ([center_x : Float ( fl/ (fl+ maxX minX) +2.0 )]
        [boxdist_x : Float ( fl/ (fl- maxX minX) 2.0 )]
        [center_y : Float ( fl/ (fl+ maxY minY) 2.0 )]
        [boxdist_y : Float( fl/ (fl- maxY minY) 2.0 )])
    (let ( [dist_x : Float (fl- px center_x)]
           [dist_y : Float (fl- py center_y) ])
      (let ([sum : Float (fl+ (fl* dist_y dist_y) (fl* dist_x dist_x))]
            [boxsum : Float (fl+ (fl* boxdist_y boxdist_y) (fl* boxdist_x boxdist_x))])
        (fl< (fl- (flsqrt sum) (flsqrt boxsum) ) rad  )))))

 
#| not yet done |#
(define (pointCorrelation [tr : Tree] [px : Float] [py : Float] [rad : Float]) : Tree
  (case tr
    [(Leaf x y c )
     (Leaf x y (if (fl<  (flsqrt (fl+ (fl* (fl- px x ) (fl- px  x))
                                      (fl* ( fl- py y ) (fl- py  y))))
                         rad)
                   (+ c 1)
                   (+ c 0)))]
    [ (Node splitAccess splitLoc minX maxX minY maxY leftChild rightChild)
       ( if  (canCorrelate minX maxX minY maxY px py rad)
             (Node splitAccess splitLoc minX maxX minY maxY
                   (pointCorrelation leftChild px py rad)
                   (pointCorrelation rightChild px py rad))
             (Node splitAccess splitLoc minX maxX minY maxY leftChild rightChild))]))

(define (pointCorrelation_v2 [tr : Tree] [px : Float][py : Float] [rad : Float]) : Int
  (case tr
    [(Leaf x y c)
     (if (fl<  (flsqrt ( fl+ (fl* ( fl- px x ) (fl- px  x)  )
                             (fl* ( fl- py y ) (fl- py  y)  ))) rad)
         1
         0
         )]
    [ (Node splitAccess splitLoc minX maxX minY maxY leftChild rightChild)
       ( if  (canCorrelate minX maxX minY maxY px py rad)
             (+ (pointCorrelation_v2 leftChild px py rad) (pointCorrelation_v2 rightChild px py rad))
             0
       )]))

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
