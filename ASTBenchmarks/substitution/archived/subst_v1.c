#include <stdio.h>
#include <stdlib.h>
#include <sexp.h>
#include <sexp_ops.h>

void walk_tree(sexp_t* sx) {
  if (sx) {
    switch(sx->ty) {
      case SEXP_VALUE:
        printf("%s ", sx->val);
        walk_tree(sx->next);
        return;
      case SEXP_LIST:
        walk_tree(sx->list);
        walk_tree(sx->next);
        return;
      default:
        return;
    }
  }
}

int main(int argc, char **argv) {
  FILE *fp;
  char *status;
  sexp_t *sx;

  // Read file as a string
  fp = fopen("/u/budkahaw/Builds/treewalk/ASTBenchmarks/expanded_racket/share/pkgs/base/info.rkt.out.sexp","r+");

  fseek(fp, 0, SEEK_END);
  long fsize = ftell(fp);
  fseek(fp, 0, SEEK_SET);  //same as rewind(f);

  char *sexp = malloc(fsize + 1);
  fread(sexp, fsize, 1, fp);
  fclose(fp);

  sexp[fsize] = 0;

  // Generate the parse tree
  if (sexp) {
    sx = parse_sexp(sexp, fsize);
  } else {
    printf("Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }

  // Print the parse tree
  int ret = print_sexp(sexp, BUFSIZ, sx);

  if (ret == -1) {
    printf("Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }

  // printf("%s\n", sexp);
  walk_tree(sx);

  destroy_sexp(sx);

  sexp_cleanup();

  exit(EXIT_SUCCESS);
}
