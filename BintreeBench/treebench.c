// A regular, pointer-based C implementation.

// This uses heap-allocation for the trees, just like the other
// benchmarks.

#include <stdio.h>
#include <stdlib.h>

// Manual layout:
// one byte for each tag, 64 bit integers
typedef long long Num;

enum Type { Leaf, Node };

// struct Tree;

typedef struct Tree {
    enum Type tag;
    union {
      struct { long long elem; };
      struct { struct Tree* l;
               struct Tree* r; };
    };
} Tree;

// Memory management
//--------------------------------------------------------------------------------

void deleteTree(Tree* t) {
  if (t->tag == Node) {
    deleteTree(t->l);
    deleteTree(t->r);
  }
  free(t);
}

#ifdef BUMPALLOC
#warning "Using bump allocator."
char* heap_ptr = 0;
// For simplicity just use a single large slab:
#define INITALLOC heap_ptr = malloc(500 * 1000 * 1000);
#define ALLOC(n) (heap_ptr += n)
// HACK, delete by rewinding:
#define DELTREE(p) { heap_ptr = (char*)p; }
#else
#define INITALLOC {}
#define ALLOC malloc
#define DELTREE deleteTree
#endif

//--------------------------------------------------------------------------------


// Helper function
Tree* fillTree(int n, Num root) {
  Tree* tr = (Tree*)ALLOC(sizeof(Tree));  
  if (n == 0) {
    tr->tag = Leaf;
    tr->elem = root;
  } else {    
    tr->tag = Node;
    tr->l = fillTree(n-1, root);; 
    tr->r = fillTree(n-1, root + (1<<(n-1)));
  }
  return tr;  
}

Tree* buildTree(int n) {
  return fillTree(n, 1);
}

void printTree(Tree* t) {
  if (t->tag == Leaf) {
    printf("%lld", t->elem);
    return;
  } else {
    printf("(");
    printTree(t->l);
    printf(",");
    printTree(t->r);
    printf(")");
    return;
  }
}

// Out-of-place add1 to leaves.
Tree* add1Tree(Tree* t) {
  Tree* tout = (Tree*)ALLOC(sizeof(Tree));
  tout->tag = t->tag;
  if (t->tag == Leaf) {
    tout->elem = t->elem + 1;
  } else {
    tout->l = add1Tree(t->l);
    tout->r = add1Tree(t->r);
  }
  return tout;
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

#include <time.h>

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

  INITALLOC;
  clock_t begin = clock();  
  Tree* tr = buildTree(depth);
  clock_t end = clock();
  double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
  printf("done building, took %lf seconds\n\n", time_spent);
  if (depth <= 5) {
    printf("Input tree:\n");
    printTree(tr); printf("\n");
  }

  if ( iters < 0 ) {
    iters = -iters;
    double trials[iters];
    for(int i=0; i<iters; i++) {
      begin = clock();
      Tree* t2 = add1Tree(tr);
      end = clock();
      time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
      if(iters < 100)
        printf("  run(%d): %lf\n", i, time_spent);
      trials[i] = time_spent;
      if (depth <= 5 && i == iters-1) {
        printf("Output tree:\n");
        printTree(t2); printf("\n");
      }
      DELTREE(t2);
    }
    qsort(trials, iters, sizeof(double), compare_doubles);
    printf("Sorted: ");
    for(int i=0; i<iters; i++)
      printf(" %lf", trials[i]);
    printf("\nMINTIME: %lf\n",    trials[0]);
    printf("MEDIANTIME: %lf\n", trials[iters / 2]);
    printf("MAXTIME: %lf\n", trials[iters - 1]);
    printf("AVGTIME: %lf\n", avg(trials,iters));
    // printTree(t2); printf("\n");
  }
  else
  {
    printf("Timing %d iters as a batch\n", iters);
    begin = clock();
    for(int i=0; i<iters; i++) {
      Tree* t2 = add1Tree(tr);
      DELTREE(t2);
    }
    end = clock();
    time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("BATCHTIME: %lf\n", time_spent);
  }
  DELTREE(tr);
  return 0;
}

