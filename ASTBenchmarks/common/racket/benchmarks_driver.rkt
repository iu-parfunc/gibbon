#lang typed/racket/base

(require racket/system
	 racket/match
	 racket/string
     racket/port
     racket/file
     racket/flonum)

(provide driver)

(define target-time 2.0)
(define ARGMAX 25)
(define MINITERS 10)

;; CSV
;; NAME, VARIANT, ARGS, ITERS, MEANTIME

;; `read-line`, `split-string`, `(match _ [(list "BATCHTIME:" t) …` right?
;; reads until it finds BATCHTIME
(define (read-batchtime [port : Input-Port]
                        [err-port : Input-Port]
                        [cmd : String]
                        [get-exit-code : (-> (U Byte False))]) : Real	 
  (define line (read-line port 'any))
  (if (eof-object? line)
      (begin (printf "Got EOF. Process returned ~s.~n" (get-exit-code))
             (printf "stderr: ~s\n" (port->string err-port))
             (printf "Command was:\n    $ ~a\n" cmd)
             (error "Error: Got eof."))
      (begin
        (let ([strs (string-split (cast line String))])
          (match strs
            [`("BATCHTIME:" ,t)
             (begin (displayln "BATCHTIME") (displayln t)
             (cast (string->number t) Real))]
            [_ ;;(begin (displayln strs) 
             (read-batchtime port err-port cmd get-exit-code)])))))

;; port that proccess wrote to
(define (get-input-port ls)
  (match ls
   [`(,ip ,op ,pid ,stde ,proc)
    ip]))

(define (get-proc ls)
  (match ls
    [`(,ip ,op ,pid ,stde ,proc)
     proc]))

(define (get-output-port ls)
  (match ls
    [`(,ip ,op ,pid ,stde ,proc)
     op]))

(define (get-error-port ls)
  (match ls
    [`(,ip ,op ,pid ,stde ,proc)
     stde]))

;; mostly stolen from bintreebench
(define (driver [csv-port : Output-Port] [exec : String] [pass-name : String]
		[variant : String]);; [file_list : String])

  (fprintf csv-port "NAME, VARIANT, ARGS, ITERS, MEANTIME\n") ;; start csv file
  
  ;; loop through all files
  (define files (file->lines "cleaned_list.txt"))
  ;;(define files (file->lines file_list))
  (define location "./cleaned_racket")

  (printf "~a files in the dataset.\n" (length files))
  
  (for ([f_ (in-list files)])
    (define f (build-path location f_))
    (printf "file ~a\n" f_)
    (let loop ([iters MINITERS])
      (printf "iters ~a\n" iters)
      (define cmd (format "~a ~a ~a" exec f iters)) ;; make sure this matches copyprop benchamrk
      (define ls (process cmd))
      (define func (get-proc ls))
      
      (define op (get-input-port ls)) ;; to close
      (define err (get-error-port ls)) ;; to close

      (define batchseconds
        (read-batchtime op err cmd (lambda ()
                                     (func 'exit-code))))
      (func 'wait) ;; wait to close ports
      (close-input-port op)
      (close-output-port (get-output-port ls))
      (close-input-port (get-error-port ls))

      (if (>= batchseconds target-time) ;; batchseconds >= 2.0
          (let ([meantime (exact->inexact (/ batchseconds iters))])
	    (printf "\nITERS: ~a\n" iters)
            (printf "BATCHTIME: ~a\n" (exact->inexact batchseconds))
            (printf "MEANTIME: ~a\n" meantime)
            (printf "Done with pass, ~a.\n" pass-name)

	    ;; write to csv
	    (fprintf csv-port "~a, ~a, ~a, ~a, ~a\n"
	  	     pass-name variant f iters meantime)
	    (flush-output csv-port))
	  (let ([multiple (max 2 (truncate (/ (fl->exact-integer target-time) (inexact->exact batchseconds))))])
            (printf "~a\n " batchseconds) 
            (flush-output)
            (loop (* iters multiple)))))
  ))
