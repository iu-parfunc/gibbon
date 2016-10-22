// Factored out main function for the different C benchmarks.

int compare_doubles (const void *a, const void *b)
{
  const double *da = (const double *) a;
  const double *db = (const double *) b;
  return (*da > *db) - (*da < *db);
}

/* int main(int argc, char** argv) { */
/*   int depth; */
/*   if (argc > 1) */
/*     depth = atoi(argv[1]); */
/*   else  */
/*     depth = 20; */
/*   printf("Building tree, depth %d\n", depth); */
/*   // CLOCK_REALTIME */
/*   clock_t begin = clock(); */
/*   TreeRef tr = buildTree(depth); */
/*   clock_t end = clock(); */
/*   double time_spent = (double)(end - begin) / CLOCKS_PER_SEC; */
/*   printf("done building, took %lf seconds\n\n", time_spent); */
/*   // printTree(tr); printf("\n");a */
/*   TreeRef t2 = malloc(treeSize(depth)); */
/*   double trials[TRIALS]; */
/*   for(int i=0; i<TRIALS; i++) { */
/*     begin = clock(); */
/*     add1Tree(tr,t2); */
/*     end = clock(); */
/*     time_spent = (double)(end - begin) / CLOCKS_PER_SEC; */
/*     printf("  run(%d): %lf\n", i, time_spent); */
/*     trials[i] = time_spent; */
/*   } */
/*   qsort(trials, TRIALS, sizeof(double), compare_doubles); */
/*   printf("Sorted: "); */
/*   for(int i=0; i<TRIALS; i++) */
/*     printf(" %lf", trials[i]);   */
/*   printf("\nSELFTIMED: %lf\n", trials[TRIALS / 2]); */
/*   // printTree(t2); printf("\n"); */
/*   free(tr); */
/*   return 0; */
/* } */

double avg(const double* arr, int n) {
  double sum = 0.0;
  for(int i=0; i<n; i++) sum += arr[i];
  return sum / (double)n;
}

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
  
  printf("Building tree, depth %d.  Benchmarking %d iters.\n", depth, iters);

  clock_t begin = clock();  
  TreeRef tr = buildTree(depth);
  clock_t end = clock();
  double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
  printf("done building, took %lf seconds\n\n", time_spent);
  if (depth <= 5) {
    printf("Input tree:\n");
    printTree(tr); printf("\n");
  }

  TreeRef t2 = malloc(treeSize(depth));
  
  if ( iters < 0 ) {
    iters = -iters;
    double trials[iters];
    for(int i=0; i<iters; i++) {
      begin = clock();
      add1Tree(tr,t2);
      end = clock();
      time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
      if(iters < 100)
        printf("  run(%d): %lf\n", i, time_spent);
      trials[i] = time_spent;
    }
    qsort(trials, iters, sizeof(double), compare_doubles);
    printf("Sorted: ");
    for(int i=0; i<iters; i++)
      printf(" %lf", trials[i]);
    printf("\nMINTIME: %lf\n",    trials[0]);
    printf("MEDIANTIME: %lf\n", trials[iters / 2]);
    printf("MAXTIME: %lf\n", trials[iters - 1]);
    printf("AVGTIME: %lf\n", avg(trials,iters));
  }
  else
  {
    printf("Timing %d iters as a batch\n", iters);
    begin = clock();
    for(int i=0; i<iters; i++) 
      add1Tree(tr,t2);
    end = clock();
    time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("BATCHTIME: %lf\n", time_spent);
  }
  if (depth <= 5) {
    printf("Output tree:\n");
    printTree(t2); printf("\n");
  }
  free(t2);
  free(tr);
  return 0;
}
