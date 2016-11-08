#! /usr/bin/env racket
#lang typed/racket/base

(require "benchmarks_driver.rkt")
(require racket/path racket/cmdline racket/file racket/system)
(require/typed racket/os [gethostname (-> String)])

(provide launch-benchmarks)

(define host (list->string (for/list ([c (gethostname)] #:unless (char-numeric? c)) c)))

(define gitdepth (read (car (process "git log --pretty=oneline | wc -l" ))))

(define destdir (format "~a/results_backup/tree-velocity/~a/~a/" 
                        (find-system-path 'home-dir) host gitdepth))
(system (format "mkdir -p ~a" destdir))
(printf "Created final output location: ~a\n" destdir)

(define PASSNAME "treebench")

;; variant is lang like treelang-racket
#|
treelang-c-packed - C backend for treelang, packed representation
treelang-racket - treelang executed through Sarah's racket embedding
treelang-c-pointer - we wont get to it for this paper, but the compiler-generated pointer-chasing code
handwritten-c-pointer
handwritten-c-packed
|#

(define (launch-benchmarks [exec : String] [pass-name : String] [variant : String])  
  (define outfile (format "results_~a_~a.csv" variant (current-seconds)))
  (define csv (open-output-file outfile #:exists 'replace))

  (driver csv exec pass-name variant)
  (system (format "cp ~a ~a" outfile (string-append destdir outfile)))
  (printf "Output copied to ~a\n" destdir)
  (close-output-port csv)) 
