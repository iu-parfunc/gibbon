
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
  // dict_t *env = NULL;

  // Read file as a string 
  fp = fopen("sexps.in","r+");

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
    printf("Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }
  
  printf("Print the parse tree, BUFSIZ=%d:\n", BUFSIZ);
  char *printed = malloc(BUFSIZ);
  int ret = print_sexp(printed, BUFSIZ, sx);
  printf("%s\n",printed);
  
  if (ret == -1) {
    printf("Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }

  // printf("%s\n", sexp);

  printf("Done printing.  Now walking parsed tree at %p...\n", sx);
  walk_tree(sx);

  destroy_sexp(sx);

  sexp_cleanup();

  exit(EXIT_SUCCESS);
}
