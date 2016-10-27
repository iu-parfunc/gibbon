


#| Grammar

prog := d ... f ... e

d := (data T [K t ...] ...)

f := (define (f [v : t] ...) : t e)

t := Int | Sym | Bool
   | (Vector t ...) 
   | (SymDict t)
   | T

e := var | lit
   | (f e ...)
   | (vector e ...)
   | (vector-ref e int)
   | (K e ...)
   | (case e [(K v ...) e] ...)
   | (let ([v t e] ...) e)
   | (if e e e)
   | primapp

primapp := (binop e e)
        | (insert e e e)
        | (lookup e e)
        | (empty-dict)

binop := + | - | * 

lit := int | #t | #f


|#


;; Example "hello world" program in this grammar:
'
(data Tree
       [Leaf Int]
       [Node Tree Tree])
'
(define (add1 [x : Tree]) : Tree
  (case x
    [(Leaf n)   (Leaf (+ n 1))]
    [(Node x y) (Node (add1 x) (add1 y))]))


;; TODO: define macros that enable code in the above language to be
;; expandable and executable in DrRacket.


