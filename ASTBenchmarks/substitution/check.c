
#include "ast.h"
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sexp.h>
#include <sexp_ops.h>

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

  printf("Generated parse tree..\n");

  printf("Print the parse tree, BUFSIZ=%d:\n", fsize);
  char *printed = malloc(fsize);
  int ret = print_sexp(printed, BUFSIZ, sx);
  printf("%s\n", printed);

  if (ret == -1) {
    fprintf(stderr,"Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }

  free(printed);

  ast_t* ast = build_ast(sx);

  // print_ast(ast);

  destroy_sexp(sx);

  sexp_cleanup();

  exit(EXIT_SUCCESS);
} 
