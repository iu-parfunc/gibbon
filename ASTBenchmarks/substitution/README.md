
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

##C Implementation

### Files
C implementation parses a file containing a s-expression and generates a packed 
in memory representation of the AST. Related files are:

* parse.h & parse.c - Implementation of parsing
* ast.h   & ast.c   - Implementation for generating pointer based in memory AST representation
* pack.h  & pack.h  - Implementation for packing the pointer based AST representation 
* check.c           - Driver file demonstrating API usage
* tests.sexp        - Test suite inputs
* debug.h           - Diagnostics
* subst.h & subst.c - In progress substitution pass on the packed representation

### API Usage

Here is a one liner for generating packed in memory representation from a file containing a s-expression.

```c
#include "pack.h"

char* fname  = "sexp.in";
char* packed = pack_ast(build_ast(parse(fname)));
```

### How to Run

`make check` builds and runs the test suite. 
`./check.sh` runs Racket data set.

### Packed Representation

Packed representation contains an interned symbol table and the pack tree itself in a single memory buffer.
Symbol table is at the head of the buffer as shown below.

<--------- Buffer ---------->
| Symbol Table | Packed Tree|

Symbol table format is as follows.

| Symbol Table Size | String1 Size | String1 | ... | StringN Size | StringN |

Packed tree format is as follows.

| Tree size | Node Tag | Node Contents ...|

AST nodes have different serialization according to their content as given below.

DefineValues & DefineSyntaxes :
  
