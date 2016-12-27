#lang gibbon

(define (fib [n : Int]) : Int
  (if (= n 2) 1
      (if (= n 1) 1
          (+ (fib (- n 1))
             (fib (- n 2))))))

(time (fib 28))
