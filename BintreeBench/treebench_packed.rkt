#lang racket

(require racket/unsafe/ops
         ffi/unsafe
         )

(define size 20)
(printf "Benchmarking on tree of size 2^~a\n" size)


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
