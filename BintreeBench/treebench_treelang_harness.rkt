#lang typed/racket/base

(require racket/system
	 "treebench_treelang.rkt")
;; Infrastructure to set up and run the benchmark.


;;
(define ARGS 1) ;; max 25
(define ARGSMAX 25)
;; double iters until batchtime is over 1 sec
(define target-time 1.0)

(define (time-add1 [size : Fixnum] [iters : Fixnum])
  (let ([tr0 : Tree (build-tree size)])
    (time-apply (lambda ()
    		  (for/list : (Listof Tree) ([_ (in-range iters)])
		    (add1-tree tr0)))
                '()))) 

(define (func)
  (for ([args (in-range 1 26)])
    (let loop ([iters : Fixnum 1])
      (collect-garbage)
      (define-values (_ cpu real gc)
        (time-add1 args iters))
		
      (define batchseconds (/ real 1000.0))
      (if (>= batchseconds target-time)
      	  (let ([meantime (exact->inexact (/ batchseconds iters))])
	    (printf "\nARGS: ~a\n" args)
	    (printf "ITERS: ~a\n" iters)
            (printf "BATCHTIME: ~a\n" (exact->inexact batchseconds))
            (printf "MEANTIME: ~a\n" meantime)
            (printf "Done with pass,.\n" ))
	  (loop (cast (* 2 iters) Fixnum))))))

(func)