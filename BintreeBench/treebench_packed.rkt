#lang racket/base

(require racket/unsafe/ops
         ffi/unsafe
         racket/match
         racket/cmdline)

(define depth
  (command-line #:args (depth)
                (or (string->number depth)
                    (error "Bad numeric argument: ~a" depth))))
(printf "Benchmarking on tree of size 2^~a\n" depth)

;; choose which version to use:
(require "./racket-packit/cursors.rkt")
(define-ops fxvector)

;; We should add macro support, like this:
#; 
(define-packed Tree
  [Node Tree Tree]
  [Leaf _llong])
#;
(define (size tc)
  (match-packed tc
   [(Node recurL recurR)
    (let ([x (recurL)]
          [y (recurR)])
      (+ x y))]
   [(Leaf read-num)
    (read-num)
    1]))


(define-tags (Leaf Node))


(define (filltree! buf offset n0)
  (define (go root n offset)
    (cond [(unsafe-fx= 0 n)
           (define offset1 (write-tag! buf offset Leaf))
           (write-int64! buf offset1 root)]
          [else
           (define off1 (write-tag! buf offset Node))
           (define off2 (go root (unsafe-fx- n 1) off1))
           (go (+ root (unsafe-fxlshift 2 (unsafe-fx- n 1)))
               (unsafe-fx- n 1)
               off2)]))
  (go 1 n0 offset))

;; Optimization; Compute the exact number of slots needed.
(define (treesize depth)
  (define leaves (expt 2 depth))
  (define nodes (sub1 leaves))
  (+ (* int64-slots leaves)
     (* tag-slots (+ nodes leaves))))

(define (add1tree! buf1 off1 buf2 off2)
  (define (go off)
    (define t (read-tag*! buf1 off))
    (cond
      [(tag=? t Leaf)
       (let* ([off2* (write-tag! buf2 off Leaf)]
              [i (read-int64*! buf1 off2*)]
              [off2** (write-int64! buf2 off2* i)])
         off2**)]
      [else (let* ([off2* (write-tag! buf2 off Node)]
                   [off2** (go off2*)])
              (go off2**))]))
    (go off1))

;; ----------------------------------------

(define-values (tr-buf tr-off) (new-buffer (treesize depth)))
(define tr-off* (filltree! tr-buf tr-off depth))

(printf "Filled buffer of size ~a\n" (buffer-size tr-buf tr-off*))

(define-values (tr2-buf tr2-off) (new-buffer (treesize depth)))

; (printf "Input tree: ~a\n" tr)
(require disassemble)
  (disassemble add1tree!)
(for ([i (in-range 10)])
  (let ([tr-off 0]
        [tr2-off 0])
    (time (add1tree! tr-buf tr-off tr2-buf tr2-off))
    ; (printf "Output tree: ~a\n" tr2)
    ))

#|

(define p0 (malloc 100))
(define p (ptr-add p0 0))

(define intsz (ctype-sizeof _int))

(ptr-set! p _byte 3)
(ptr-add! p 1 _byte)
(ptr-set! p _int 4)
(ptr-add! p intsz _byte)
(ptr-set! p _int 5)

(printf "Done making unaligned writes, now reading.\n")

(set! p (ptr-add p0 0))
(display (ptr-ref p _byte)) (newline)
(ptr-add! p 1 _byte)
(display (ptr-ref p _int)) (newline)
(ptr-add! p intsz _byte)
(display (ptr-ref p _int)) (newline)



(display "FINISHME\n")
(exit 1)
|#
