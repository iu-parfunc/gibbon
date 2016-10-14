
The calling convention for each implementation of this benchmark
program is:

  prog <symbol> <input-file> <iterations>

The program should

 (1) parse the input (Racket expanded core language) file,
 (2) put the AST into the designated in-memory representation
 (3) run a substitution tree-walk replacing unbound occurrences of
     <symbol> with any new symbol, e.g. "<symbol>1".
     Repeat the tree-walk for <iterations> total repetitions.

The substitution pass can stop when it reaches a binding site for
<symbol>, although it is permitted to traverse the tree (pointlessly),
it ais not permitted to perform substitution under such a shadowing
binding.

Example invocation:

    ./subst.rkt call-with-values ../expanded_racket/share/pkgs/srfi-lib/srfi/45/lazy.rkt.out.sexp 10

And here's a big (16MB) one from dataset 1.2:

    ./subst.rkt extend-parameterization ../expanded_racket/share/pkgs/macro-debugger-text-lib/macro-debugger/model/reductions.rkt.out.sexp 10

