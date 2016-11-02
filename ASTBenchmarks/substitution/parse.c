
#include "parse.h"
#include "debug.h"
#include <stdio.h>
#include <stdlib.h>

sexp_t* parse(char* fname) {
  FILE *fp;
  sexp_t *sx;

  fp = fopen(fname,"r+");

  if (fp <= 0) {
    ERROR("[PARSE] Failed to open file..");
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
    free(sexp);
  } else {
    ERROR("[PARSE] Error parsing the s-expression : %d", sexp_errno);
  }

  return sx;
}
