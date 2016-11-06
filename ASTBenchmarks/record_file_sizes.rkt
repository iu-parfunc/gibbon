#! /usr/bin/env racket
#lang racket

(define files (file->lines "cleaned_list.txt"))

(printf "~a files in the dataset.\n" (length files))

(define sum 0)
(with-output-to-file "packed_sizes.csv"
  (lambda ()
    (printf "NAME,PACKEDSIZE\n")
    (for ([f (in-list files)])
      (define sz (file-size f))
      (set! sum (+ sz sum))
      (printf "~a,~a\n" f sz)))
  #:exists 'replace)

(printf "All file sizes written.  Total bytes, ~a.\n" sum)
