// A regular, pointer-based C implementation.

// This uses heap-allocation for the trees, just like the other
// benchmarks.

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define TRIALS 17

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

/*

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

*/

// Out-of-place add1 to leaves.
Tree* add1Tree(Tree* t) {
  Tree* tout = (Tree*)ALLOC(sizeof(Tree));
  tout->tag = t->tag;
  if (t->tag == Leaf) {
    tout->elem = t->elem;
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


int main(int argc, char** argv) {
  int depth;
  if (argc > 1)
    depth = atoi(argv[1]);
  else 
    depth = 20;
  printf("Building tree, depth %d\n", depth);

  INITALLOC;
  clock_t begin = clock();  
  Tree* tr = buildTree(depth);
  clock_t end = clock();
  double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
  printf("done building, took %lf seconds\n\n", time_spent);
  // printTree(tr); printf("\n");a
  double trials[TRIALS];
  for(int i=0; i<TRIALS; i++) {
    begin = clock();
    Tree* t2 = add1Tree(tr);
    end = clock();
    time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("  run(%d): %lf\n", i, time_spent);
    trials[i] = time_spent;
    DELTREE(t2);
  }
  qsort(trials, TRIALS, sizeof(double), compare_doubles);
  printf("Sorted: ");
  for(int i=0; i<TRIALS; i++)
    printf(" %lf", trials[i]);
  printf("\nSELFTIMED: %lf\n", trials[TRIALS / 2]);
  // printTree(t2); printf("\n");
  DELTREE(tr);
  return 0;
}

