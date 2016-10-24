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

(define (add1-tree tr)
  (if (pair? tr)
      (cons (add1-tree (car tr))
            (add1-tree (cdr tr)))
      (unsafe-fx+ 1 tr)))

(define (bench tr n)
  (collect-garbage #;'major)
  (time (add1-tree tr)))

(define-values (size iters)
  (match (current-command-line-arguments)
    [(vector s i) (values (string->number s) (string->number i))]
    [args (error "expected two command line args, <depth> <iters>, got ~a:\n  ~a"
                 (vector-length args))]))

(printf "Benchmarking on tree of size 2^~a, ~a iterations\n" size iters)
(time
 (let ([tr (build-tree size)])
   (for ((i (in-range iters)))
     (bench tr size))))
