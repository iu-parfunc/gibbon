#include <time.h>

// Factored out main function for the different C benchmarks.

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

int main(int argc, char** argv) {
  int depth, iters;
  if (argc > 2) {
    depth = atoi(argv[1]);
    iters = atoi(argv[2]);
  } else {
    fprintf(stderr,"Expected two arguments, <depth> <iters>\n");
    fprintf(stderr,"Iters can be negative to time each iteration rather than all together\n");
    abort();
  }
  
  printf("  Building tree, depth %d.  Benchmarking %d iters.\n", depth, iters);

#ifdef PARALLEL
  printf("THREADS: %d\n", __cilkrts_get_nworkers());
#endif

  struct timespec begin, end;
  clock_gettime(which_clock, &begin);
  TreeRef tr = buildTree(depth);
  clock_gettime(which_clock, &end);
  double time_spent = difftimespecs(&begin, &end);
  
  printf("  done building, took %lf seconds\n\n", time_spent);
  if (depth <= 5) {
    printf("  Input tree:\n  ");
    printTree(tr); printf("\n");
  }

  TreeRef t2 = malloc(treeSize(depth));
  
  if ( iters < 0 ) {
    iters = -iters;
    double trials[iters];
    for(int i=0; i<iters; i++) {
      clock_gettime(which_clock, &begin);
      add1Tree(tr,t2);
      clock_gettime(which_clock, &end);
      time_spent = difftimespecs(&begin, &end);
      if(iters < 100)
        printf("    run(%d): %lf\n", i, time_spent);
      trials[i] = time_spent;
    }
    qsort(trials, iters, sizeof(double), compare_doubles);
    printf("  Sorted: ");
    for(int i=0; i<iters; i++)
      printf(" %lf", trials[i]);
    printf("\nMINTIME: %lf\n",    trials[0]);
    printf("MEDIANTIME: %lf\n", trials[iters / 2]);
    printf("MAXTIME: %lf\n", trials[iters - 1]);
    printf("AVGTIME: %lf\n", avg(trials,iters));
  }
  else
  {
    printf("  Timing %d iters as a batch\n", iters);
    clock_gettime(which_clock, &begin);
    for(int i=0; i<iters; i++) 
      add1Tree(tr,t2);
    clock_gettime(which_clock, &end);
    time_spent = difftimespecs(&begin, &end);
    printf("BATCHTIME: %lf\n", time_spent);
  }
  if (depth <= 5) {
    printf("  Output tree:\n  ");
    printTree(t2); printf("\n");
  }
  free(t2);
  free(tr);
  return 0;
}

