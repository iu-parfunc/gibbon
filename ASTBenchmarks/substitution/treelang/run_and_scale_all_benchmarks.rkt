#! /usr/bin/env racket
#lang typed/racket

(require "parse.rkt"
         "subst_treelang.rkt"
         (only-in "../../grammar_racket.sexp" Toplvl))

(require/typed racket/os
               [gethostname ( -> String)])

(define target-time 1.0)
(define oldsym 'call-with-values)

(define-values (dir)
  (match (current-command-line-arguments)
    [(vector d) (values d)]
    [args (error "unexpected number of command line arguments, expected <input-directory>, got:\n"
                 args)]))

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

(define destdir (format "~a/results_backup/tree-velocity/~a/~a/" (find-system-path 'home-dir) host gitdepth))
(system (format "mkdir -p ~a" destdir))
(printf "Created final output location: ~a\n" destdir)

(define outfile (format "results_~a.csv" (current-seconds)))
(define csv (open-output-file outfile #:exists 'replace))
(fprintf csv "NAME, VARIANT, ARGS, ITERS, MEANTIME\n")


(for ([relative (in-list all-files)])

  (define file (build-path dir relative))

  ;; copied exactly + type annotations
  (printf "\n\nBenchmark: Substituting symbol ~a in file ~a ...\n" oldsym relative)
  (printf "============================================================\n")

  (define ast : Toplvl
     (time (parse (read (open-input-file file)))))
  (printf "Done ingesting AST.\n")
    
  (let loop ([iters 1])
    (define newsym (string->symbol (string-append (symbol->string oldsym) "99")))
    (define-values (_ cpu real gc)
      (begin (collect-garbage) ;(collect-garbage)(collect-garbage)
             (time-apply (lambda () (for ([_ (in-range iters)])
                                      (subst oldsym newsym ast)))
                         '())))
    (define batchseconds (/ real 1000.0))

    (if (>= batchseconds target-time)
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

