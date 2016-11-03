#! /usr/bin/env racket
#lang typed/racket/base

;; A tool to build shell scripts that drive benchmarking.

(require "bench_harness.rkt")
(require racket/path racket/cmdline racket/file racket/system)
(require/typed racket/os [gethostname (-> String)])
;; For Racket 6.6, not necessary in 6.7:
(require/typed racket/path [path-get-extension (Path-For-Some-System -> String)])

(provide launch-benchmarks)

(define oldsym 'call-with-values) ;; Hardcode this, doesn't matter.

(define target-time : (Parameter Real)    (make-parameter 1.0))
(define skip-to     : (Parameter Integer) (make-parameter 0))

(define dir
  (command-line
   #:program "run_and_scale_all_benchmarks"
   #:once-each
   [("-s" "--skip-to") #{N : String} "Start benchmarking with file number N"
    (skip-to (cast (string->number N) Integer))]
   [("-t" "--target-time") #{T : String}
    "Increase iteration count until a batch takes T time, in seconds"
    (target-time (cast (string->number T) Real))]
   #;#;#:usage-help ;; usage-help doesn't work in Typed Racket :(
   "Expects one argument, the directory to scan for .sexp files.\n"
   #:args (#{input-directory : String}) ; expect one command-line argument: <filename>
   input-directory))

(define all-files 
  (parameterize ([current-directory dir])
    (find-files (lambda ([p : Path]) 
                  ; (printf "Testing ~a ,ext ~a\n" p (path-get-extension (cast p Path-For-Some-System)))
                  (equal? (format "~a" (path-get-extension p)) ".sexp"))
                "."
                ; #:skip-filtered-directory? #f
                )))

(printf "Found ~a files\n" (length all-files))

(define host (list->string (for/list ([c (gethostname)] #:unless (char-numeric? c)) c)))

(define gitdepth (read (car (process "git log --pretty=oneline | wc -l" ))))

(define destdir (format "~a/results_backup/tree-velocity/~a/~a/" 
                        (find-system-path 'home-dir) host gitdepth))
(system (format "mkdir -p ~a" destdir))
(printf "Created final output location: ~a\n" destdir)

(define outfile (format "results_~a.csv" (current-seconds)))
(define csv (open-output-file outfile #:exists 'replace))

;; Runs benchmarks, wraps up, and exits the process.
(: launch-benchmarks : (-> Toplvl Integer Toplvl) String String -> Any)
(define (launch-benchmarks iterate-pass pass-name variant)
  (run-benchmarks csv iterate-pass pass-name variant (target-time)
                  dir all-files (skip-to))
  ; (copy-file outfile (string-append destdir outfile))
  (system (format "cp ~a ~a" outfile (string-append destdir outfile)))
  (printf "Output copied to ~a\n" destdir)
  (close-output-port csv)
  (exit 0))
