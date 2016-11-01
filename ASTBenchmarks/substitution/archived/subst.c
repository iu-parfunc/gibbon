
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sexp.h>
#include <sexp_ops.h>

int compare_doubles (const void *a, const void *b)
{
  const double *da = (const double *) a;
  const double *db = (const double *) b;
  return (*da > *db) - (*da < *db);
}

double avg(const double* arr, int n) {
  double sum = 0.0;
  for(int i=0; i<n; i++) sum += arr[i];
  return sum / (double)n;
}

double difftimespecs(struct timespec* t0, struct timespec* t1) {
  return (double)(t1->tv_sec - t0->tv_sec)
    + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
}

static clockid_t which_clock = CLOCK_MONOTONIC_RAW;

sexp_t* substitute(sexp_t* sx, char* from, char* to) {
  if (sx) {
    sexp_t* sx_new = (sexp_t*) calloc(1, sizeof(sexp_t));
    memcpy(sx_new, sx, sizeof(sexp_t));

    switch(sx->ty) {
      case SEXP_VALUE:
        {

          sx_new->next = substitute(sx->next, from, to);
          if (strcmp(sx->val, from) == 0) {
            sx_new->val = to;
          }

          return sx_new;
        }
      case SEXP_LIST:
        {
          sx_new->list = substitute(sx->list, from, to);
          sx_new->next = substitute(sx->next, from, to);
          return sx_new;

        }
      default:
        printf("Invalid parse tree..\n");
        exit(EXIT_FAILURE);
    }
  }
}

int main(int argc, char **argv) {  
  FILE *fp;
  int iterations = 1;
  char *status, *fn, *from, *to = "something-else";
  sexp_t *sx;
  double* times;

  // Read file as a string  
  if (argc > 2) {
    from = argv[1]; 
    fn = argv[2];
    iterations = atof(argv[3]);
  } else {
    from = ""; 
    fn = "sexps.in";
  }

  times = (double*) calloc(iterations, sizeof(double));

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

  printf("Generated parse tree..\n");

  /* 
  printf("Print the parse tree, BUFSIZ=%d:\n", fsize);
  char *printed = malloc(fsize);
  int ret = print_sexp(printed, BUFSIZ, sx);
  printf("%s\n",printed);

  if (ret == -1) {
    fprintf(stderr,"Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }

  free(printed);
  */

  struct timespec begin, end;

  sexp_t* sx_new;
  for (int i=0; i < iterations; i++) {
    clock_gettime(which_clock, &begin);
    sx_new = substitute(sx, from, to);
    clock_gettime(which_clock, &end);
    times[i] = difftimespecs(&begin, &end);

    printf("    run(%d): %lf\n", i, times[i]);
  }

  printf("Ran substitution for %d iterations\n", iterations);

  /*
  printf("Parse tree after substitution, BUFSIZ=%d:\n", fsize);
  printed = malloc(fsize);
  ret = print_sexp(printed, BUFSIZ, sx_new);
  printf("%s\n",printed);
  */

  qsort(times, iterations,  sizeof(double), compare_doubles);
  printf("  Sorted: ");
  for(int i=0; i< iterations; i++)
    printf(" %lf", times[i]);
  printf("\nMINTIME: %lf\n",    times[0]);
  printf("MEDIANTIME: %lf\n", times[iterations / 2]);
  printf("MAXTIME: %lf\n", times[iterations - 1]);
  printf("AVGTIME: %lf\n", avg(times, iterations));

  printf("\n --------------------------------------------------------------\n");

  // print_ast(ast);

  destroy_sexp(sx);

  sexp_cleanup();

  exit(EXIT_SUCCESS);
}
