#lang racket

(require racket/unsafe/ops
         ffi/unsafe
         )
(require "./racket-packit/cursors.rkt")

(define depth
  (match (current-command-line-arguments)
    [(vector s) (or (string->number s)
                    (error "Bad numeric argument: ~a" s))]
    [args (error "expected one command line arg, <depth>, got ~a:\n  ~a"
                 (vector-length args))]))
(printf "Benchmarking on tree of size 2^~a\n" depth)


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

(define (filltree! c n0)
  (define (go root n)
    (if (zero? n)
        (begin (write-tag! c Leaf)
               (write-int64! c root))
        (begin (write-tag! c Node)
               (go root (sub1 n))               
               (go (+ root (expt 2 (sub1 n)))
                   (sub1 n)))))
    (define off0 (cursor-offset c))
    (go 1 n0)
    off0)

;; Optimization; Compute the exact number of slots needed.
(define (treesize depth)
  (define leaves (expt 2 depth))
  (define nodes (sub1 leaves))
  (+ (* int64-slots leaves)
     (* tag-slots (+ nodes leaves))))

(define (add1tree! c1 c2)
  (define t (read-tag! c1))
  (cond
    [(tag=? t Leaf) (write-tag! c2 Leaf)
                    (write-int64! c2 (read-int64! c1))]
    [else (write-tag! c2 Node)
          (add1tree! c1 c2)
          (add1tree! c1 c2)]))

;; ----------------------------------------

(define tr (new-buffer (treesize depth)))
(set-cursor-offset! tr (filltree! tr depth))

(printf "Filled buffer of size ~a\n" (buffer-size tr))

(define tr2 (new-buffer (treesize depth)))

; (printf "Input tree: ~a\n" tr)

(for ([i (range 10)])
  (set-cursor-offset! tr  0)
  (set-cursor-offset! tr2 0)
  (time (add1tree! tr tr2))
  ; (printf "Output tree: ~a\n" tr2)
  )

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
