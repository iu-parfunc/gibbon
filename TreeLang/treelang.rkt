#lang typed/racket

(provide Int Sym Bool SymDict data empty-dict lookup insert case
         define let provide require if :
         or and
         vector vector-ref
         and empty? error 
         eq? = True False

         time + * -

         #%app #%module-begin #%datum quote
         only-in all-defined-out ann #%top-interaction
         #;(all-from-out typed/racket))

(require (prefix-in r typed/racket/base)
         (for-syntax syntax/parse)
         racket/performance-hint
         racket/unsafe/ops)

;; add for/list  w/types

#| Grammar

prog := #lang s-exp "/path/to/treelang.rkt"
        d ... f ... e

;; Data definitions:
d := (data T [K fTy ...] ...)

;; Function definitions
f := (define (f [v : t] ...) : t e)

;; Field types
fTy := t | (Listof t)

;; Types
t := Int | Sym | Bool
   | (Vector t ...) 
   | (SymDict t) ;; maps symbols to t?
   | T

e := var | lit
   | (f e ...)                      
   | (vector e ...)                 
   | (vector-ref e int)             
   | (K e ...)                      
   | (case e [(K v ...) e] ...)
   | (let ([v : t e] ...) e)        :: CHANGED THIS, note the :
   | (if e e e)                     
   | primapp
   | (for/list ([v : t e])  e)      ;; not enforced that only loops over single list
   | (error string)
   | (time e)                       ;; time a benchmark

primapp := (binop e e)
        | (insert e e e)
        | (lookup e e)
        | (empty-dict)

binop := + | - | * 

lit := int | #t | #f

;; Example "hello world" program in this grammar:

(data Tree
       [Leaf Int]
       [Node Tree Tree])
'
(define (add1 [x : Tree]) : Tree
  (case x
    [(Leaf n)   (Leaf (+ n 1))]
    [(Node x y) (Node (add1 x) (add1 y))]))

|#

;; CONSIDERING, but not aded yet:
;;        | (dict-size e)


;;(case e [(K v ...) e] ...)
(define-syntax (case stx)
  (syntax-parse stx
    [(case v [(~and pat (S:id p:id ...)) rhs] ...)
     (syntax/loc stx
       (match v
         [pat rhs] ...))]))

;;(insert e e e)
(define-syntax-rule (insert ht key v)
  (hash-set ht key v))

(define-syntax-rule (lookup ht key)
  (hash-ref ht key))

(define-syntax-rule (empty-dict)
  (hash))

(define-syntax-rule (time e)
  (let-values ([(ls cpu real gc) (time-apply (lambda () e) '())])
    (printf "SELFTIMED: ~a\n" (/ (exact->inexact real) 1000.0))
    (match ls
      [(list x) x])))

(define-type Int Fixnum)
(define-type Sym Symbol)
(define-type Bool Boolean)
(define-type (SymDict t) (HashTable Symbol t))

(define-syntax (data stx)
  (syntax-case stx ()
    [(_ type1 [ts f ...] ...)
     (with-syntax ([((f-ids ...) ...)
                    (map generate-temporaries (syntax->list #'((f ...) ...)))])
       #'(begin
           (define-type type1 (U ts ...))
           (struct ts ([f-ids : f] ...) #:transparent) ...))]))

(define True #t)
(define False #f)

(begin-encourage-inline 
  ;; FIXME: need to make sure these inline:
  (define (+ [a : Int] [b : Int]) : Int
    (unsafe-fx+ a b))
  
  (define (- [a : Int] [b : Int]) : Int
    (unsafe-fx- a b))
  
  (define (* [a : Int] [b : Int]) : Int
    (unsafe-fx* a b))

  (define (eq? [a : Sym] [b : Sym]) : Bool
    (req? a b))
  
  (define (= [a : Int] [b : Int]) : Bool
    (req? a b))
  )

#|
(data Tree
       [Leaf Int]
       [Node Tree Tree])

(define (add1 [x : Tree]) : Tree
  (case x
    [(Leaf n)   (Leaf (+ n 1))]
    [(Node x y) (Node (add1 x) (add1 y))]))
|#
                                  


