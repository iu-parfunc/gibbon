
;; Chez Scheme version

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
              (go root (sub1 n)))))
  (go 1 n))

(define (add1-tree tr)
  (if (pair? tr)
      (cons (add1-tree (car tr))
            (add1-tree (cdr tr)))
      (#3%fx+ 1 tr)))

(define (sum-tree tr)
  (if (pair? tr)
      (+ (sum-tree (car tr))
	 (sum-tree (cdr tr)))
      tr))

(define (bench tr n)
  (collect-maximum-generation)
  (time (add1-tree tr)))

(define-values (mode size iters)
  (cond 
   [(= (length (command-line-arguments)) 3)
    (values (car (command-line-arguments)) 
	    (string->number (cadr  (command-line-arguments)))
            (string->number (caddr (command-line-arguments))))]
   [else (error 'treebench (format "expected three command line args, <mode> <depth> <iters>, got ~a:  ~a"
                                   (length (command-line-arguments))
                                   (command-line-arguments)))]))

(printf "Benchmarking on tree of size 2^~a, ~a iterations\n" size iters)

(cond
 [(equal? "build" mode)
  (printf "Benchmarking build\n")
  (time
   (let* ([start-time (current-time 'time-duration)]
	  [st-ns (+ (* (expt 10 9) (time-second start-time))
		    (time-nanosecond start-time))])
     (time (let loop ([i 0])
	     (build-tree2 size)
	     (when (< i iters)
		   (loop (add1 i)))))
     (let* ([end-time (current-time 'time-duration)]
	    [et-ns (+ (* (expt 10 9) (time-second end-time))
		      (time-nanosecond end-time))]
	    [diff (- et-ns st-ns)]
	    [batchseconds (/ diff 1000000000.0)]) ;; make sure to print out a real number.
       (printf "Time ~a\n" batchseconds)
       (printf "BATCHTIME: ~a\n" batchseconds)
       )))
  ]
 [(equal? "sum" mode)
  (printf "Benchmarking sum\n")
  (time
   (let ([tr (build-tree size)])
     (let* ([start-time (current-time 'time-duration)]
	    [st-ns (+ (* (expt 10 9) (time-second start-time))
		      (time-nanosecond start-time))])
       (time (let loop ([i 0])
	       (sum-tree tr)
	       (when (< i iters)
		     (loop (add1 i)))))
       (let* ([end-time (current-time 'time-duration)]
	      [et-ns (+ (* (expt 10 9) (time-second end-time))
			(time-nanosecond end-time))]
	      [diff (- et-ns st-ns)]
	      [batchseconds (/ diff 1000000000.0)]) ;; make sure to print out a real number.
	 (printf "Time ~a\n" batchseconds)
	 (printf "BATCHTIME: ~a\n" batchseconds)
	 ))))
  ]
 [else
  (printf "Benchmarking add1\n")
  (time
   (let ([tr (build-tree size)])
     (let* ([start-time (current-time 'time-duration)]
	    [st-ns (+ (* (expt 10 9) (time-second start-time))
		      (time-nanosecond start-time))])
       (time (let loop ([i 0])
	       (add1-tree tr)
	       (when (< i iters)
		     (loop (add1 i)))))
       (let* ([end-time (current-time 'time-duration)]
	      [et-ns (+ (* (expt 10 9) (time-second end-time))
			(time-nanosecond end-time))]
	      [diff (- et-ns st-ns)]
	      [batchseconds (/ diff 1000000000.0)]) ;; make sure to print out a real number.
	 (printf "Time ~a\n" batchseconds)
	 (printf "BATCHTIME: ~a\n" batchseconds)
	 ))))
  ])

