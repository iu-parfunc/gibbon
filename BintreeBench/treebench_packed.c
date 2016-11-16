// A manual implementation of a single-buffer, packed bintree
// representation and a treewalk of it.

#include <malloc.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

enum Mode { Build, Sum, Add1 };

enum Tree {
    Leaf,
    Node,
};

// Manual layout:
// one byte for each tag, 64 bit integers
typedef char* TreeRef;
typedef long long Num;

// Helper function
TreeRef fillTree(TreeRef cursor, int n) {
  // printf("  filltree: %p, n=%d, fill=%lld", cursor, n, root); fflush(stdout);
  if (n == 0) {
    *cursor = Leaf;
    cursor++;
    *((Num*)cursor) = 1; // Unaligned!
    // printf("; wrote tag %d, payload %lld\n", Leaf, root); fflush(stdout);
    return (cursor + sizeof(Num));
  } else {
    *cursor = Node;

    // Padding for a 4-byte offset changes perf from ~3.5ms to 4ms on 2^20 nodes:
    // cursor += 4;

    // printf("; wrote tag %d\n", Node); fflush(stdout);
    TreeRef tout = fillTree(cursor + 1, n - 1);
    return fillTree(tout, n - 1);
  }
}

int treeSize(int n) {
  int leaves = 1 << n;
  int nodes  = leaves - 1;
  // Both nodes and leaves are tagged:
  int bytes  = (sizeof(Num)*leaves + sizeof(char)*(nodes+leaves));
  /* printf("treeSize(%d): %d bytes (%d/%d nodes/leaves)\n", */
  /*        n, bytes, nodes, leaves); */
  return bytes;
}

TreeRef buildTree(int n) {
  int bytes = treeSize(n);
  char* buf = malloc(bytes);
  char* res = fillTree(buf, n);
  printf("wrote %d bytes in buildTree\n", (int)(res - buf));
  return buf;
}

TreeRef printTree(TreeRef t) {
  if (*t == Leaf) {
    t++;
    printf("%lld", *(Num*)t);
    return (t+sizeof(Num));
  } else {
    t++;
    printf("(");
    TreeRef t2 = printTree(t);
    printf(",");
    TreeRef t3 = printTree(t2);
    printf(")");
    return t3;
  }
}

TreeRef add1Tree(TreeRef t, TreeRef tout) {
  if (*t == Leaf) {
    *tout = Leaf;
    TreeRef t2    = t    + 1;
    TreeRef tout2 = tout + 1;
    *(Num*)tout2 = *(Num*)t2 + 1;
    return (t2 + sizeof(Num));
  } else {
    *tout = Node;

    TreeRef t2    = t    + 1;
    TreeRef tout2 = tout + 1;

    TreeRef t3    = add1Tree(t2, tout2);
    TreeRef tout3 = tout2 + (t3 - t2);
    return add1Tree(t3, tout3);
  }
}

Num sumTree(TreeRef t, TreeRef* tout)
{
    TreeRef t1 = t + 1; // skip tag
    char tag = *t;
    if (tag == Leaf)
    {
        *tout = t1 + sizeof(Num);
        return *(Num*)t1;
    }
    else
    {
        TreeRef tout1;
        Num sum1 = sumTree(t1, &tout1);
        Num sum2 = sumTree(tout1, tout);
        return sum1 + sum2;
    }
}

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

void bench_single_pass(TreeRef tr, int depth, int iters)
{
    struct timespec begin, end;

    iters = -iters;
    double trials[iters];
    for (int i=0; i<iters; i++)
    {
        clock_gettime(which_clock, &begin);
        TreeRef tout = malloc(treeSize(depth));
        TreeRef t2 = add1Tree(tr, tout);
        free(t2);
        clock_gettime(which_clock, &end);
        double time_spent = difftimespecs(&begin, &end);
        if(iters < 100) {
            printf(" %lld", (long long)(time_spent * 1000));
            fflush(stdout);
        }
        trials[i] = time_spent;
    }
    qsort(trials, iters, sizeof(double), compare_doubles);
    printf("\nSorted: ");
    for(int i=0; i<iters; i++)
        printf(" %d",  (int)(trials[i] * 1000));
    printf("\nMINTIME: %lf\n",    trials[0]);
    printf("MEDIANTIME: %lf\n", trials[iters / 2]);
    printf("MAXTIME: %lf\n", trials[iters - 1]);
    printf("AVGTIME: %lf\n", avg(trials,iters));
}

void bench_add1_batch(TreeRef tr, int depth, int iters)
{
    struct timespec begin, end;

    printf("Timing iterations as a batch\n");
    printf("ITERS: %d\n", iters);
    clock_gettime(which_clock, &begin);
    for (int i=0; i<iters; i++)
    {
        TreeRef tout = malloc(treeSize(depth));
        TreeRef t2 = add1Tree(tr, tout);
        free(tout);
    }
    clock_gettime(which_clock, &end);
    malloc_stats();
    double time_spent = difftimespecs(&begin, &end);
    printf("BATCHTIME: %lf\n", time_spent);
}

void bench_build_batch(int depth, int iters)
{
    struct timespec begin, end;
    TreeRef t2;

    printf("BUILD: Timing iterations as a batch\n");
    printf("ITERS: %d\n", iters);
    clock_gettime(which_clock, &begin);
    for (int i=0; i<iters; i++)
    {
      t2 = buildTree(depth);
      free(t2);
    }
    clock_gettime(which_clock, &end);
    malloc_stats();
    double time_spent = difftimespecs(&begin, &end);
    printf("BATCHTIME: %lf\n", time_spent);
}

void bench_sum_batch(TreeRef tr, int iters)
{
    struct timespec begin, end;
    Num sum;
    printf("SUM: Timing iterations as a batch\n");
    printf("ITERS: %d\n", iters);

    clock_gettime(which_clock, &begin);
    for (int i=0; i<iters; i++)
    {
        TreeRef tout;
        sum = sumTree(tr, &tout);
    }
    clock_gettime(which_clock, &end);

    printf("Final sum of leaves: %lld \n", sum);
    double time_spent = difftimespecs(&begin, &end);
    printf("BATCHTIME: %lf\n", time_spent);
}

int main(int argc, char** argv)
{
    char* modestr; // first arg
    int depth;     // second arg
    int iters;     // third arg
    enum Mode mode;

    if (argc <= 3)
    {
        fprintf(stderr,"Expected three arguments, <build|add1|sum> <depth> <iters>\n");
        fprintf(stderr,"Iters can be negative to time each iteration rather than all together\n");
        exit(1);
    }

    modestr = argv[1];
    depth = atoi(argv[2]);
    iters = atoi(argv[3]);

    printf("Benchmarking in mode: %s\n", modestr);

    if (!strcmp(modestr, "sum"))   mode = Sum;
    else if (!strcmp(modestr, "build")) mode = Build;
    else if (!strcmp(modestr, "add1"))  mode = Add1;
    else { printf("Error: unrecognized mode.\n"); exit(1); }

    printf("SIZE: %d\n", depth);
    printf("Building tree, depth %d.  Benchmarking %d iters.\n", depth, iters);

    struct timespec begin, end;
    clock_gettime(which_clock, &begin);
    TreeRef tr = buildTree(depth);
    clock_gettime(which_clock, &end);
    double time_spent = difftimespecs(&begin, &end);
    printf("Done building input tree, took %lf seconds\n\n", time_spent);
    if (depth <= 5)
    {
        printf("Input tree:\n");
        printTree(tr); printf("\n");
    }

    printf("Running traversals (ms): ");

    if (iters < 0)
    {
      bench_single_pass(tr, depth, iters);
      free(tr);
    }
    else
      {
      switch(mode) {
      case Add1:
	bench_add1_batch(tr, depth, iters);
	free(tr);
        break;
      case Sum:
	bench_sum_batch(tr, iters);
	free(tr);
        break;
      case Build:
	free(tr);
	bench_build_batch(depth, iters);
        break;
    default: printf("Internal error\n"); exit(1);
    }
    }
    return 0;
}
