
#include "ast.h"
#include "pack.h"
#include "parse.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sexp.h>
#include <sexp_ops.h>

void compare_sexp(sexp_t* sx, sexp_t* sxnew) {
  if (sx && sxnew) {
    assert(sx->ty == sxnew->ty);
    switch(sx->ty) {
      case SEXP_VALUE:
        assert(strcmp(sx->val, sxnew->val) == 0);
        compare_sexp(sx->next, sxnew->next);
        break;
      case SEXP_LIST:
        compare_sexp(sx->list, sxnew->list);
        compare_sexp(sx->next, sxnew->next);
        break;
      default:
        printf("Invalid parse tree..\n");
        exit(EXIT_FAILURE);
    }
  } else {
    assert(sx == sxnew);
  }
}

int main(int argc, char **argv) {  
  int iterations = 1;
  char *fname;
  sexp_t *sx;

  // Read file as a string  
  if (argc > 2) {
    fname = argv[1]; 
    iterations = atoi(argv[2]);
  } else {
    printf("Invalid arguments to check. Usage : ./check <input_file> iterations\n");
    exit(EXIT_FAILURE);
  }

  printf("[Parse Tree Operations]\n\n");
  printf("Generating the parse tree..\n");

  // Parse
  sx = parse(fname);

  printf("\n[AST Operations]\n\n");
  printf("Generating the AST.\n");

  // Build AST
  ast_t* ast = build_ast(sx);

  printf("Serializing the AST..\n");

  // Serialize AST back to a s-expression for validating
  print_ast(ast, "sexp.out");
  sexp_t* sxnew = parse("sexp.out");

  printf("\n[Validation]\n\n");
  printf("Validating AST serialization..\n");

  // Validate AST serialization with the original s-expression
  compare_sexp(sx, sxnew);
  printf("SUCCESS!!\n");

  printf("\n[Packing]\n\n");
  printf("Packing the AST.\n");
  pack_ast(ast);

  destroy_sexp(sx);
  destroy_sexp(sxnew);

  sexp_cleanup();

  exit(EXIT_SUCCESS);
} 
