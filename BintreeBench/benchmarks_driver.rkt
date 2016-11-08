#lang typed/racket/base

(require racket/system
	 racket/match
	 racket/string
     racket/port)

(provide driver)

(define target-time 1.0)
(define ARGMAX 25)

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
             (printf "stderr: ~s~n" (port->string err-port))
             (printf "Command was:~n    $ ~a~n" cmd)
             (error "Error: Got eof."))
      (begin
        (let ([strs (string-split (cast line String))])
          (match strs
            [`("BATCHTIME:" ,t)
             (cast (string->number t) Real)]
            [_ (read-batchtime port err-port cmd get-exit-code)])))))

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

(define (driver [csv-port : Output-Port] [exec : String] [pass-name : String]
		[variant : String])
  (fprintf csv-port "NAME, VARIANT, ARGS, ITERS, MEANTIME\n") ;; start csv file
  
  ;; loop through args 1 to 25
  (for ([args (in-range 1 (+ 1 ARGMAX))])
    (printf "ARGS: ~a\n" args)
    (let loop ([iters 1])
      (define cmd (format "~a ~a ~a" exec args iters))
      (define ls (process cmd))
      (define block_func (get-proc ls))
      (block_func 'wait)
      (define op (get-input-port ls))
      (define err (get-error-port ls))

      (define batchseconds
        (read-batchtime op err cmd (lambda () (block_func 'exit-code))))
      (close-input-port op)
      (close-output-port (get-output-port ls))
      (close-input-port (get-error-port ls))

      (if (>= batchseconds target-time)
          (let ([meantime (exact->inexact (/ batchseconds iters))])
	    (printf "\nITERS: ~a\n" iters)
            (printf "BATCHTIME: ~a\n" (exact->inexact batchseconds))
            (printf "MEANTIME: ~a\n" meantime)
            (printf "Done with pass, ~a.\n" pass-name)

	    ;; write to csv
	    (fprintf csv-port "~a, ~a, ~a, ~a, ~a\n"
	  	     pass-name variant args iters meantime)
	    (flush-output csv-port)
	    )
	  (begin (printf "~a " batchseconds) (flush-output)
	         (loop (* 2 iters)))))))
