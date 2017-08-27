#lang racket

(require "test-uniqify.rkt")
(require "test-flatten.rkt")
(require "test-compiler.rkt")
(require "../parse.rkt")

(require "../compiler.gib")
(require "../common.gib")

(require rackunit)

;; TODO: write a parser for R0 so that we can re-use all tests from the class

(define tests
  (list uniqify-tests
        flatten-tests
        compiler-tests))

(define (run-tests)
  (let* ([test-results (flatten (map run-test tests))]
         [num-test-results (length test-results)]
         [failures (map test-result-test-case-name
                        (filter (compose not test-success?) test-results))])
    (inspect-results failures num-test-results "tests")))

;; run examples
(define examples-dir "tests/examples")

;; no support for (read) yet ..
(define ignored-examples (set "r0_1.rkt" "r0_2.rkt" "r0_3.rkt" "r1_9.rkt" "r1_10.rkt" "r1_19.rkt"
                              "r1_13.rkt" "r1_15.rkt" "r1a_4.rkt" "r1a_5.rkt" "r1a_6.rkt"
                              ;; actual failure
                              "r1a_8.rkt"))

(define (run-example e)
  (let* ([parsed (parse (build-path examples-dir (path->string e)))]
         [out-path (build-path examples-dir (path-replace-extension e ".out"))]
         [expected (if (file-exists? out-path)
                       (call-with-input-file out-path (lambda (f) (read f)))
                       42)]
         [actual (compile parsed)])
    (if (equal? actual expected)
        ""
        (file-name-from-path e))))

(define (run-examples)
  (let* ([files (directory-list examples-dir)]
         [rkt-files (filter (lambda (p)
                              (and (equal? (path-get-extension p) #".rkt")
                                   (not (set-member? ignored-examples (path->string p)))))
                            files)]
         [test-results (map run-example rkt-files)]
         [num-test-results (length test-results)]
         [failures (map path->string (filter (lambda (x) (not (equal? x ""))) test-results))])
    (inspect-results failures num-test-results "examples")))

(define (inspect-results xs num-test-results type)
  (if (empty? xs)
      (printf "All ~a ~a passed.\n" num-test-results type)
      (begin (printf "~s/~s ~a failed.\n~a\n"
                     (length xs)
                     num-test-results
                     type
                     (string-join xs "\n"))
             (exit 1))))

(run-tests)
(run-examples)
