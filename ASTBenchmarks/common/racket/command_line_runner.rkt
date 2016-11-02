#! /usr/bin/env racket
#lang racket

;; A tool to build shell scripts that drive benchmarking.

(require "bench_harness.rkt")
(require racket/os racket/path)
;(require/typed racket/os [gethostname ( -> String)] )
;; For Racket 6.6, not necessary in 6.7:
; (require/typed racket/path [path-get-extension (Path-For-Some-System -> String)] )

(provide launch-benchmarks)

(define oldsym 'call-with-values) ;; Hardcode this, doesn't matter.

(define target-time (make-parameter 1.0))
(define skip-to     (make-parameter 0))

(define dir ; : Path-String
  (command-line
   #:program "run_and_scale_all_benchmarks"
   #:once-each
   [("-s" "--skip-to") N "Start benchmarking with file number N"
    (skip-to (string->number N))]
   [("-t" "--target-time") T
    "Increase iteration count until a batch takes T time, in seconds"
    (target-time (string->number T))]
   #:usage-help
   "Expects one argument, the directory to scan for .sexp files.\n"
   #:args (input-directory) ; expect one command-line argument: <filename>
   input-directory))

(define all-files 
  (parameterize ([current-directory dir])
    (find-files (lambda (p) 
                  ; (printf "Testing ~a ,ext ~a\n" p (path-get-extension (cast p Path-For-Some-System)))
                  (equal? (format "~a" (path-get-extension
                                        p #;(cast p Path-For-Some-System))) ".sexp"))
                "."
                ; #:skip-filtered-directory? #f
                )))

(printf "Found ~a files\n" (length all-files))

(define host (list->string 
              (filter (lambda (c) (not (char-numeric? c #;(cast c Char))))
                      (string->list (gethostname)))))

(define gitdepth (read (car (process "git log --pretty=oneline | wc -l" ))))

(define destdir (format "~a/results_backup/tree-velocity/~a/~a/" 
                        (find-system-path 'home-dir) host gitdepth))
(system (format "mkdir -p ~a" destdir))
(printf "Created final output location: ~a\n" destdir)

(define outfile (format "results_~a.csv" (current-seconds)))
(define csv (open-output-file outfile #:exists 'replace))

;; Runs benchmarks, wraps up, and exits the process.
(define (launch-benchmarks iterate-pass pass-name variant)
  (run-benchmarks csv iterate-pass pass-name variant (target-time)
                  dir all-files (skip-to))
  ; (copy-file outfile (string-append destdir outfile))
  (system (format "cp ~a ~a" outfile (string-append destdir outfile)))
  (printf "Output copied to ~a\n" destdir)
  (close-output-port csv)
  (exit 0))
