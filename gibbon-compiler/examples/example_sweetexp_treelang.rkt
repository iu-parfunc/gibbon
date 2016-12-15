#lang sweet-exp "./gibbon/gibbon.rkt"

(require (only-in racket bytes-length))
; define x 3
; printf("~a\n" x)


data Tree
  Leaf Int
  Node Tree Tree

let ([x : Tree  Leaf(3)])
  case x
    Leaf(n)   99
    Node(x y) 100

 ; bytes-length(pack-Tree(x))


;; (data Tree
;;   [Leaf Int]
;;   [Node Tree Tree])

;; (let ([x : Tree (Leaf 3)])
;;   (bytes-length (pack-Tree x)))
