
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sexp.h>
#include <sexp_ops.h>

void substitute(sexp_t* sx, char* from, char* to) {
  if (sx) {
    switch(sx->ty) {
      case SEXP_VALUE:
      {
        if (strcmp(sx->val, from) == 0) {
          sx->val = to;
        }

        substitute(sx->next, from, to);
        break;
      }
      case SEXP_LIST:
      {
        substitute(sx->list, from, to);
        substitute(sx->next, from, to);
        break;
      }
      default:
        printf("Invalid parse tree..\n");
        exit(EXIT_FAILURE);
    }
  }
}

int main(int argc, char **argv) {  
  FILE *fp;
  char *status, *fn, *from, *to = "BOO!!";
  sexp_t *sx;

  // Read file as a string  
  if (argc > 2) {
    from = argv[1]; 
    fn = argv[2];
  } else {
    from = ""; 
    fn = "sexps.in";
  }

  printf("Opening file: %s\n", fn);
  fp = fopen(fn,"r+");

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
  
  printf("Print the parse tree, BUFSIZ=%d:\n", fsize);
  char *printed = malloc(fsize);
  int ret = print_sexp(printed, BUFSIZ, sx);
  printf("%s\n",printed);
  
  if (ret == -1) {
    fprintf(stderr,"Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }

  free(printed);

  substitute(sx, from, to);

  printf("Parse tree after substitution, BUFSIZ=%d:\n", fsize);
  printed = malloc(fsize);
  ret = print_sexp(printed, BUFSIZ, sx);
  printf("%s\n",printed);

  printf("%s\n", sx);
  // ast_list_t* ast = build_ast(sx, true);

  // print_ast(ast);

  // destroy_sexp(sx);

  sexp_cleanup();

  exit(EXIT_SUCCESS);
}
