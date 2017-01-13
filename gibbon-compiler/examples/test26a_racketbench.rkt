#lang typed/racket

;; A little harness to make it convenient to run treewalk on a file manually.
;; Takes <size> <iters> on the command line and reads a file name of stdin.
;;  (Ignores <size>...)


(require "../../ASTBenchmarks/treewalk/gibbon/treewalk_gibbon.rkt")
(require "../../ASTBenchmarks/common/racket/parse.rkt")
(require gibbon)

(let ((f (read-line)))
   (if (eof-object? f)
       (error "Expects file name on stdin!\n")
       (let ((p (parse (file->value f))))
         (iterate (treewalk p))
         'done)))
