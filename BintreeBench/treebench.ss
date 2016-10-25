
;; Chez Scheme version

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
      (#3%fx+ 1 tr)))

(define (bench tr n)
  (collect-maximum-generation)
  (time (add1-tree tr)))

(define-values (size iters)
  (cond 
   [(= (length (command-line-arguments)) 2)
    (values (string->number (car  (command-line-arguments)))
            (string->number (cadr (command-line-arguments))))]
   [else (error 'treebench (format "expected two command line args, <depth> <iters>, got ~a:  ~a"
                                   (length (command-line-arguments))
                                   (command-line-arguments)))]))

(printf "Benchmarking on tree of size 2^~a, ~a iterations\n" size iters)
(time
 (let ((tr (build-tree size)))
  (let loop ((i 1))
    (bench tr size)
    (when (< i 10)
          (loop (add1 i))))))
