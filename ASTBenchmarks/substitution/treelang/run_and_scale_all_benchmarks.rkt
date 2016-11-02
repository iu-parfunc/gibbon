#! /usr/bin/env racket
#lang typed/racket/no-check

(require "parse.rkt"
         "subst_treelang.rkt"
         (only-in "../../grammar_racket.sexp" Toplvl))

(require/typed racket/os
               [gethostname ( -> String)])
;; For Racket 6.6, not necessary in 6.7:
(require/typed racket/path
               [path-get-extension (Path-For-Some-System -> String)])

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
                  (equal? (format "~a" (path-get-extension (cast p Path-For-Some-System))) ".sexp"))
                "."
                ; #:skip-filtered-directory? #f
                )))

(printf "Found ~a files\n" (length all-files))

(define host (list->string 
              (filter (lambda (c) (not (char-numeric? (cast c Char))))
                      (string->list (gethostname)))))

(define gitdepth (read (car (process "git log --pretty=oneline | wc -l" ))))

(define destdir (format "~a/results_backup/tree-velocity/~a/~a/" 
                        (find-system-path 'home-dir) host gitdepth))
(system (format "mkdir -p ~a" destdir))
(printf "Created final output location: ~a\n" destdir)

(define outfile (format "results_~a.csv" (current-seconds)))
(define csv (open-output-file outfile #:exists 'replace))
(fprintf csv "NAME, VARIANT, ARGS, ITERS, MEANTIME\n")

(define fileNum 1)
(for ([relative (in-list all-files)])

  (define file (build-path dir relative))

  ;; copied exactly + type annotations
  (printf "\n\nBenchmark(~a): Substituting symbol ~a in file ~a ...\n" fileNum oldsym relative)
  (set! fileNum (add1 fileNum))
  (printf "============================================================\n")

  (define ast : Toplvl
     (time 
      (let* ([port (open-input-file file)]
             [res  (parse (read port))])
        (close-input-port port)
        res)))
  (printf "Done ingesting AST.\n")
    
  (let loop ([iters 1])
    (define newsym (string->symbol (string-append (symbol->string oldsym) "99")))
    (define-values (_ cpu real gc)
      (begin (collect-garbage) ;(collect-garbage)(collect-garbage)
             (time-apply (lambda () (for ([_ (in-range iters)])
                                      (subst oldsym newsym ast)))
                         '())))
    (define batchseconds (/ real 1000.0))

    (if (>= batchseconds (target-time))
        (let ((meantime (exact->inexact (/ batchseconds iters))))
          (printf "\nITERATIONS: ~a\n" iters)
          (printf "BATCHTIME: ~a\n" (exact->inexact batchseconds))
          (printf "MEANTIME: ~a\n" meantime)
          (printf "Done with substitution pass.\n")

          (fprintf csv "substitution, treelang-racket, ~a, ~a, ~a\n"
                   relative iters meantime)
          (flush-output csv)
          (printf "Output written to: ~a\n" outfile)
          ; (copy-file outfile (string-append destdir outfile))
          (system (format "cp ~a ~a" outfile (string-append destdir outfile)))
          (printf "Output copied to ~a\n" destdir)
          )
        (begin (printf "~a " batchseconds) (flush-output)
               (loop (* 2 iters)))))
) ;; End loop.

(close-output-port csv)
