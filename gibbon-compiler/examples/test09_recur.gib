#lang gibbon

(define (fib [n : Int]) : Int
  (if (= n 2) 1
      (if (= n 1) 1
          (+ (fib (- n 1))
             (fib (- n 2))))))

;; Reducing this for DEBUG=1 execution where we interpret after every pass:
;; (time (fib 28))
(time (fib 10))
