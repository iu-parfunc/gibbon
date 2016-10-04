// A manual implementation of a single-buffer, packed bintree
// representation and a treewalk of it.

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define TRIALS 17
#define TAG_SIZE 1

// Should be at least log_2(numProcs)
#define PARLVLS 0
#define MINSEQLVLS 1

#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y))

enum Tree {
    Leaf,
    Node,
    Indirect,
};

// Manual layout:
// one byte for each tag, 64 bit integers
typedef char* TreeRef;
typedef long long Num;

// Helper function
TreeRef fillTree(TreeRef cursor, int n, Num root) {
  // printf("  filltree: %p, n=%d, fill=%lld", cursor, n, root); fflush(stdout);
  if (n == 0) {
    *cursor = Leaf;
    cursor++;    
    *((Num*)cursor) = root; // Unaligned!
    // printf("; wrote tag %d, payload %lld\n", Leaf, root); fflush(stdout);
    return (cursor + sizeof(Num));
  } else {
    *cursor = Node;

    // Padding for a 4-byte offset changes perf from ~3.5ms to 4ms on 2^20 nodes:
    // cursor += 4; 
    
    // printf("; wrote tag %d\n", Node); fflush(stdout);
    char* cur2 = fillTree(cursor+1, n-1, root);
    return fillTree(cur2, n-1, root + (1<<(n-1)));
  }
}

int treeSize(int n) {
  int leaves = 1 << n;
  int nodes  = leaves - 1;
  // Both nodes and leaves are tagged:
  int bytes  = sizeof(Num)*leaves + TAG_SIZE * sizeof(char) * (nodes+leaves);
  return bytes;
}

      
TreeRef buildSeqTree(int n, Num root) {
  int bytes = treeSize(n);
  char* buf = malloc(bytes);
  char* res = fillTree(buf, n, root);
  return buf;
}

TreeRef buildParTree(int p, int n, Num root) {
  if (p == 0) return buildSeqTree(n,root);
  char* start = malloc(3 * TAG_SIZE + 2 * sizeof(void*));
  char* buf = start;
  buf[0] = Node;
  buf[1] = Indirect;
  buf += 2;
  ((void**)buf)[0] = buildParTree(p-1, n, root);
  buf += sizeof(void*);
  buf[0] = Indirect;
  buf += 1;
  ((void**)buf)[0] = buildParTree(p-1, n, root + (1<<((p+n)-1)));
  return start;
}

TreeRef buildTree(int n) {
  if (n >= MINSEQLVLS) {
    int p  = MIN(PARLVLS, n - MINSEQLVLS);
    int n2 = n - p;
    return buildParTree(p, n2, 1);
  } else {
    return buildSeqTree(n, 1);
  }
}

// Returns the cursor position AFTER scrolling past the input argument.
TreeRef printTree(TreeRef t) {
  switch(*t) {
  case Leaf:
    t++;
    printf("%lld", *(Num*)t);
    return (t+sizeof(Num));

  case Node:
    t++;
    printf("(");
    TreeRef t2 = printTree(t);
    printf(",");
    TreeRef t3 = printTree(t2);
    printf(")");
    return t3;
    
  case Indirect:
    t++;
    printTree(*((TreeRef*)t));
    // Then scroll past the pointer and return.
    return (t + sizeof(void*));

  default:
    fprintf(stderr, "Error! corrupt tree in printTree, at %p, expected tag got: %d\n", t, (*t));
    exit(-1);
    return 0;
  }
}

typedef struct { TreeRef tin; TreeRef tout; } RefPair;

TreeRef add1Tree(TreeRef t, TreeRef tout) {
  // printf("Add1tree %p -> %p, ", t, tout);
  switch(*t) {
  case Leaf:
    // printf(" leaf %lld\n", *(Num*)(t+1));
    *tout = Leaf;    
    t++; tout++;
    *(Num*)tout = *(Num*)t + 1;
    return (t+sizeof(Num));

  case Node:
    // printf(" node\n");
    *tout = Node;
    t++; tout++;

    // Padding experiment
    // t += 4;
    // tout += 4;
    
    TreeRef t2 = add1Tree(t,tout);
    tout += (t2 - t); // HACK: FIXME - Won't work with indirections.
    return add1Tree(t2,tout);

  // Here we have a choice, we could keep the indirection or inline
  // it.  For starters let's just inline.  We'll need to compute a
  // length of the output buffer to create if we don't.
  case Indirect:
    // fprintf(stderr, "Indirect, tout = %p\n", tout);
    t++;
    TreeRef tnew = *((TreeRef*)t);
    add1Tree(tnew, tout);
    return t + sizeof(void*);
    
  default:
    fprintf(stderr, "Corrupt tree in add1Tree, at %p expected tag got: %d\n", t, (*t));
    exit(-1);
    return 0;
  }
}

int compare_doubles (const void *a, const void *b)
{
  const double *da = (const double *) a;
  const double *db = (const double *) b;
  return (*da > *db) - (*da < *db);
}

int main(int argc, char** argv) {

  TreeRef ta = buildTree(3);
  TreeRef tb = malloc(treeSize(3));
  printf("Allocated tb space: %d, %p \n", treeSize(3), tb);
  add1Tree(ta, tb);
  printTree(ta);
  printf("\ntb:\n");
  printTree(tb);  
  return 0;
  
  int depth;
  if (argc > 1)
    depth = atoi(argv[1]);
  else 
    depth = 20;
  printf("Building tree, depth %d\n", depth);
  // CLOCK_REALTIME
  clock_t begin = clock();
  TreeRef tr = buildTree(depth);
  clock_t end = clock();
  double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
  printf("done building, took %lf seconds\n\n", time_spent);
  // printTree(tr); printf("\n");a
  TreeRef t2 = malloc(treeSize(depth));
  double trials[TRIALS];
  for(int i=0; i<TRIALS; i++) {
    begin = clock();
    add1Tree(tr,t2);
    end = clock();
    time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
    printf("  run(%d): %lf\n", i, time_spent);
    trials[i] = time_spent;
  }
  qsort(trials, TRIALS, sizeof(double), compare_doubles);
  printf("Sorted: ");
  for(int i=0; i<TRIALS; i++)
    printf(" %lf", trials[i]);  
  printf("\nSELFTIMED: %lf\n", trials[TRIALS / 2]);
  // printTree(t2); printf("\n");
  free(tr);
  return 0;
}
