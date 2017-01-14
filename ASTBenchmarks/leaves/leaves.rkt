#lang racket

(module+ main
  (require racket/cmdline)
  (define size-param 15)

  (define leaf-size (make-parameter 1))
  (define output    (make-parameter "out.sexp"))
  (define bench     (make-parameter "add1"))
  (define lang      "#lang gibbon")

  (define tree (lambda () `(data Tree
                      [Leaf ,@(make-list (string->number (leaf-size)) 'Int)]
                      [Node Tree Tree])))

  (define build-tree (lambda () `(define (build-tree [n : Int]) : Tree
                        (if (= n 0)
                          (Leaf ,@(make-list (string->number (leaf-size)) '1))
                          (let ([l : Tree (build-tree (- n 1))]
                                [r : Tree (build-tree (- n 1))])
                            (Node l r))))))

  (define add1-tree (lambda () `(define (add1-tree [tr : Tree]) : Tree
                       (case tr
                         [(Leaf ,@(make-list (string->number (leaf-size)) 'n)) 
                            (Leaf ,@(make-list (string->number (leaf-size)) '(+ 1 n)))]
                         [(Node x y) (Node (add1-tree x)
                                           (add1-tree y))]))))

  (define iter-add1 (lambda () 
                       `(let ([tr : Tree (build-tree ,size-param)])
                         (let ([a : Tree (iterate (add1-tree tr))]) 0))))

  (define iter-build (lambda () `(let ([a : Tree (iterate (build-tree ,size-param))]) 0)))

  (define (flush out)
      (printf "Writing to ~a.\n" out)
      (define outp (if out (open-output-file out) (current-output-port)))
      (write-string lang outp)
      (newline outp)
      (write (tree) outp)
      (newline outp)
      (newline outp)
      (write (build-tree) outp)
      (newline outp)
      (newline outp)
      (write (add1-tree) outp)
      (newline outp)
      (newline outp)
      (if (equal? (bench) "add1")
        (write (iter-add1) outp)
        (write (iter-build) outp))
      (newline outp)
      (newline outp)
      )

  (command-line #:program "blah"

                #:once-each
                [("-l" "--leaf-size") leaves "Leaf size" (leaf-size leaves)]
                [("-o" "--output") outf "Output" (output outf)]
                [("-b" "--bench") benchmark "Benchmark" (bench benchmark)]
                )
  (printf "Output ~a Leaf size ~a Benchmark ~a.\n" (output) (leaf-size) (bench))
  (flush (output)))
