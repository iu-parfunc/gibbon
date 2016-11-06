#lang typed/racket/base

(require racket/system)
;; Infrastructure to set up and run the benchmark.

;;
(define ARGS 1) ;; max 25
(define ARGSMAX 25)
;; double iters until batchtime is over 1 sec
(define target-time 1.0) 

(define (func)
  (for ([args (in-range 1 26)])
    (let loop ([iters 1])
      (collect-garbage)
      (define-values (_ cpu real gc)
        (time-apply (lambda () (system (format "../../racket/bin/racket ./treebench_treelang.rkt ~v ~v" args iters)))
		    '()))
      (define batchseconds (/ real 1000.0))
      (if (>= batchseconds target-time)
      	  (let ([meantime (exact->inexact (/ batchseconds iters))])
	    (printf "\nARGS: ~a\n" args)
	    (printf "ITERS: ~a\n" iters)
            (printf "BATCHTIME: ~a\n" (exact->inexact batchseconds))
            (printf "MEANTIME: ~a\n" meantime)
            (printf "Done with pass,.\n" ))
	  (loop (* 2 iters))))))

(func)