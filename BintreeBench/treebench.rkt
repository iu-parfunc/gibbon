#lang racket

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

(define (bench n)
  (define tr (build-tree n))
  (collect-garbage 'major)
  (time (add1-tree tr)))

(define size
  (match (current-command-line-arguments)
    [(vector s) (string->number s)]
    [args (error "expected one command line arg, <depth>, got ~a:\n  ~a"
                 (vector-length args))]))

(printf "Benchmarking on tree of size 2^~a\n" size)
(let loop ((i 1))
  (bench size)
  (when (< i 10)
        (loop (add1 i))))
