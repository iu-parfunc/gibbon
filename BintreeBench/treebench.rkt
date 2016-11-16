#lang racket/base

(require racket/match)

(require racket/unsafe/ops)

(define (build-tree n)
  (define (go root n)
    (if (zero? n)
        root
        (cons (go root (sub1 n))
              (go (+ root (expt 2 (sub1 n)))
                  (sub1 n)))))
  (go 1 n))

(define (build-tree2 n)
  (define (go root n)
    (if (zero? n)
        root
        (cons (go root (sub1 n))
              (go root
                  (sub1 n)))))
  (go 1 n))

(define (add1-tree tr)
  (if (pair? tr)
      (cons (add1-tree (car tr))
            (add1-tree (cdr tr)))
      (unsafe-fx+ 1 tr)))

(define (sum-tree tr)
  (if (pair? tr)
      (+ (sum-tree (car tr))
         (sum-tree (cdr tr)))
      tr))


(define (bench-one tr n)
  (displayln (sum-tree tr))
  (collect-garbage #;'major)
  (define tr* (time (add1-tree tr)))
  (displayln (sum-tree tr*))
  )

;; which
;; - sum
;; - add1
;; - build

(define-values (which size iters)
  (match (current-command-line-arguments)
    [(vector v s i) (values v (string->number s) (string->number i))]
    [args (error "expected three command line args, <which-bench> <depth> <iters>, got ~a:\n  ~a"
                 (vector-length args))]))

(printf "Benchmarking on tree of size 2^~a, ~a iterations\n" size iters)
(printf "SIZE: ~a\n" size)
(printf "ITERS: ~a\n" iters)

(cond
  [(equal? "build" which)
   (begin
     (define tree #f)
     (define-values (_ cpu real gc)
       (time-apply (λ ()
                     (for ([i (in-range iters)])
                       (set! tree (build-tree2 size))))
                   '()))
     (define batchseconds (exact->inexact (/ real 1000.0)))
     (printf "BATCHTIME: ~a\n" batchseconds)
     (printf "MEANTIME: ~a\n" (exact->inexact (/ batchseconds iters))))]
  [(equal? "sum" which)
   (let ([tr (build-tree size)])
     (collect-garbage)
     (sum-tree tr) ;; Throw one away.
     (define final #f)
     (define-values (_ cpu real gc)
       (time-apply (lambda ()
                     (for ((i (in-range iters)))                    
                       (set! final (sum-tree tr)))) '()))
     (define batchseconds (exact->inexact (/ real 1000.0)))
     (printf "BATCHTIME: ~a\n" batchseconds)
     (printf "MEANTIME: ~a\n" (exact->inexact (/ batchseconds iters)))
     (displayln (sum-tree final)))]
  [else
   (let ([tr (build-tree size)])
     (collect-garbage)
     (add1-tree tr) ;; Throw one away.
     (define final #f)
     (define-values (_ cpu real gc)
       (time-apply (lambda ()
                     (for ((i (in-range iters)))                    
                       (set! final (add1-tree tr)))) '()))
     (define batchseconds (exact->inexact (/ real 1000.0)))
     (printf "BATCHTIME: ~a\n" batchseconds)
     (printf "MEANTIME: ~a\n" (exact->inexact (/ batchseconds iters)))
     (displayln (sum-tree final)))])
