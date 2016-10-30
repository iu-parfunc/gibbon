
#include "ast.h"
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
  FILE *fp;
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

  fp = fopen(fname,"r+");

  if (fp <= 0) {
    fprintf(stderr,"Error: Failed to open file.\n");
    exit(EXIT_FAILURE);
  }

  fseek(fp, 0, SEEK_END);
  long fsize = ftell(fp);
  fseek(fp, 0, SEEK_SET);  //same as rewind(f);

  char *sexp = malloc(fsize + 1);
  int ec = fread(sexp, fsize, 1, fp);
  fclose(fp);

  sexp[fsize] = 0;

  // Generate the parse tree
  if (sexp) {
    sx = parse_sexp(sexp, fsize);
  } else {
    fprintf(stderr,"Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }

  printf("[Parse Tree Operations]\n\n");
  printf("Generating the parse tree..\n");

  printf("Serialized parse tree, BUFSIZ=%d:\n", fsize);
  char *printed = malloc(fsize);
  int ret = print_sexp(printed, BUFSIZ, sx);
  printf("%s\n", printed);

  if (ret == -1) {
    fprintf(stderr,"Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }

  free(printed);

  printf("\n[AST Operations]\n\n");
  printf("Generating the AST.\n");

  ast_t* ast = build_ast(sx);

  printf("Serializing the AST..\n");

  fp = fopen("sexp.out", "w+");

  if (fp <= 0) {
    fprintf(stderr,"Error: Failed to open file.\n");
    exit(EXIT_FAILURE);
  }

  print_ast(ast, fp);

  fseek(fp, 0, SEEK_END);
  fsize = ftell(fp);
  fseek(fp, 0, SEEK_SET);  //same as rewind(f);

  sexp = malloc(fsize + 1);
  ec = fread(sexp, fsize, 1, fp);
  fclose(fp);

  sexp[fsize] = 0;

  printf("Serialized AST, BUFSIZ=%d:\n", fsize);
  printf("%s\n", sexp);

  sexp_t* sxnew;
  // Generate the new parse tree
  if (sexp) {
    sxnew = parse_sexp(sexp, fsize);
  } else {
    fprintf(stderr,"Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }

  printf("\n[Validation]\n\n");
  printf("Validating AST serialization..\n");
  compare_sexp(sx, sxnew);
  printf("SUCCESS!!\n");

  destroy_sexp(sx);
  destroy_sexp(sxnew);

  sexp_cleanup();

  exit(EXIT_SUCCESS);
} 
