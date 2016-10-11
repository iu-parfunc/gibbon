// A manual implementation of a single-buffer, packed bintree
// representation and a treewalk of it.

#include <stdio.h>
#include <stdlib.h>
#include <map>
#include <list>
#include <time.h>
#include <cilk/cilk.h>

#define TRIALS 17
#define TAG_SIZE 1

// Should be at least log_2(numProcs)
#define PARLVLS 3
#define MINSEQLVLS 1

#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y))

using namespace std;

enum Tree {
    // Tree node related tags
    Leaf,
    Node,
    Indirect,
    // Buffer chaining related tags
    BufferSize,
    Next
};

// Manual layout:
// one byte for each tag, 64 bit integers
typedef char* TreeRef;
typedef long long Num;

map<TreeRef, list<char>> buffer_map;

// Memory allocation
typedef struct {
  TreeRef start;
  TreeRef end;
  TreeRef current;
  TreeRef next;
} TreeBuf;

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

    /*
     auto it = buffer_map.find(cursor);
     if (it != buffer_map.end()) {
       list<char> clist = it->second;
       clist.push_back(*(cursor - 1- sizeof(Num)));
       clist.push_back((char)*(cursor - sizeof(Num)));
       // clist.push_back(*ptr);
       buffer_map[buf.start] = clist;
     } else {
       list<char> clist;
       clist.push_back(*(cursor - 1- sizeof(Num)));
       clist.push_back((char)*(cursor - sizeof(Num)));
       // clist.push_back(*ptr);
       buffer_map[buf.start] = clist;
     }
     */

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

      
TreeRef buildSeqTree(int n, Num root, TreeBuf buf) {
  // int bytes = treeSize(n);
  // char* buf = malloc(bytes);
  char* res = fillTree(buf.current, n, root);
  return buf.start;
}

TreeRef buildParTree(int p, int n, Num root, TreeBuf buf) {
  // char* start = malloc(2 * TAG_SIZE + sizeof(void*));
  if (buf.current == buf.end) {
    if (buf.start < buf.end) {
      long buffer_size = (2*TAG_SIZE + sizeof(void*)) * p + 
          treeSize(n) + TAG_SIZE + sizeof(long) + TAG_SIZE + sizeof(TreeRef);
      TreeBuf newbuf;
      newbuf.start = (TreeRef) malloc(buffer_size);
      newbuf.current = newbuf.start;
      newbuf.end = newbuf.start + buffer_size; 
      newbuf.next = newbuf.end - sizeof(TreeRef);

      *((TreeRef)buf.next) = Next;
      ((TreeRef*)buf.next)[1] = newbuf.start; 

      buf = newbuf;

      *(buf.start) = BufferSize;
      *(long*)(buf.start + 1) = buffer_size;

      buf.current += (sizeof(long) + 1);
      printf("Chaining buffers..\n");
    } else {
      long buffer_size = (2*TAG_SIZE + sizeof(void*)) * p + 
          treeSize(n) + TAG_SIZE + sizeof(long) + TAG_SIZE + sizeof(TreeRef);
      buf.start = (TreeRef) malloc(buffer_size);
      buf.current = buf.start;
      buf.end = buf.start + buffer_size; 
      buf.next = buf.end - sizeof(TreeRef);

      *(buf.start) = BufferSize;
      *(long*)(buf.start + 1) = buffer_size;
      
      buf.current += (sizeof(long) + 1);
      printf("Allocating buffer at %p with size %d\n", buf.start, buffer_size);  
    }
  } 

  if (p == 0) { 
    buildSeqTree(n,root, buf);
    return buf.start;
  }

  TreeRef ptr = buf.current;
  ptr[0] = Node;
  ptr[1] = Indirect;
  ptr += 2;
  // printf("Creating new buffer at depth %d\n", p);
  TreeBuf newbuf = {0};
  *((TreeRef*)ptr) = buildParTree(p-1, n, root, newbuf);
  ptr += sizeof(void*);

  // ptr[0]  = Node; 

  auto it = buffer_map.find(buf.start);
  if (it != buffer_map.end()) {
    list<char> clist = it->second;
    clist.push_back(*(ptr-2 - sizeof(void*)));
    clist.push_back(*(ptr-1 - sizeof(void*)));
    // clist.push_back(*ptr);
    buffer_map[buf.start] = clist;
  } else {
    list<char> clist;
    clist.push_back(*(ptr-2 - sizeof(void*)));
    clist.push_back(*(ptr-1 - sizeof(void*)));
    // clist.push_back(*ptr);
    buffer_map[buf.start] = clist;
  }

  // ptr++;
  buf.current = ptr;
  buildParTree(p-1, n, root + (1<<((p+n)-1)), buf);

  // buf[0] = Indirect;
  // buf += 1;
  // ((void**)buf)[0] = buildParTree(p-1, n, root + (1<<((p+n)-1)));

  return buf.start;
}

TreeRef buildTree(int n) {
  if (n >= MINSEQLVLS) {
    int p  = MIN(PARLVLS, n - MINSEQLVLS);
    int n2 = n - p;
    TreeBuf newbuf = {0};
    TreeRef ref = buildParTree(p, n2, 1, newbuf);
    printf("Tree root is : %p\n", ref);
    return ref;
  } else {
    TreeBuf newbuf = {0};
    return buildSeqTree(n, 1, newbuf);
  }
}

TreeRef printTree(TreeRef t) {
  switch(*t) {
  case Leaf:
    t++;
    printf("%lld", *(Num*)t);
    return (t+sizeof(Num));

  case Node:
    t++;
    printf("(");
    TreeRef ref1 = printTree(t);
    printf(",");
    TreeRef ref2 = printTree(ref1);
    printf(")");
    // return t3;
    return ref2;
    
  case Indirect:
    t++;
    printTree(*((TreeRef*)t));
    // Then scroll past the pointer and return.
    return (t + sizeof(void*));
    // return;

  case BufferSize:
    t++; // Skip the tag
    t += sizeof(long); // Skip the buffer size
    // TreeRef start = (TreeRef) malloc(*(long*)t);
    printTree(t);
    return 0; 
    
  case Next:
    t++;
    return printTree(*((TreeRef*)t));

  default:
    fprintf(stderr, "Error! corrupt tree in printTree, at %p, expected tag got: %d\n", t, (*t));
    exit(-1);
    // return 0;
    return 0;
  }
}

typedef struct { TreeRef tin; TreeRef tout; } RefPair;

RefPair add1Tree(TreeRef t, TreeRef tout) {
  // printf("Add1tree %p -> %p, ", t, tout);
  switch(*t) {
  case Leaf:
    // printf(" leaf %lld\n", *(Num*)(t+1));
    *tout = Leaf;    
    t++; tout++;
    *(Num*)tout = *(Num*)t + 1;
    RefPair ret1 = {t+sizeof(Num), tout+sizeof(Num)};
    return ret1;
    // return {t+sizeof(Num), tout+sizeof(Num)} ;

  case Node:
    // printf(" node\n");
    *tout = Node;
    t++; tout++;

    // Padding experiment
    // t += 4;
    // tout += 4;
    
    RefPair refs1 = add1Tree(t,tout);
    // tout += (t2 - t); // HACK: FIXME - Won't work with indirections.
    return add1Tree(refs1.tin,refs1.tout);

  // Here we have a choice, we could keep the indirection or inline
  // it.  For starters let's just inline.  We'll need to compute a
  // length of the output buffer to create if we don't.
  case Indirect:
    // fprintf(stderr, "Indirect, tout = %p\n", tout);
    t++;
    TreeRef tnew = *((TreeRef*)t);
    RefPair refs2 = add1Tree(tnew, tout);
    RefPair ret2 = {t + sizeof(void*), refs2.tout};
    return ret2;
    
  default:
    fprintf(stderr, "Corrupt tree in add1Tree, at %p expected tag got: %d\n", t, (*t));
    exit(-1);
    RefPair fault = {0, 0};
    return fault;
  }
}

int compare_doubles (const void *a, const void *b)
{
  const double *da = (const double *) a;
  const double *db = (const double *) b;
  return (*da > *db) - (*da < *db);
}

int main(int argc, char** argv) {

  TreeRef ta = buildTree(4);
  TreeRef tb = (TreeRef) malloc(treeSize(3));
  printf("Allocated tb space: %d, %p \n", treeSize(3), tb);

  for (auto& it : buffer_map) {
    list<char> clist = it.second;
    printf("Buffer : %p\n", it.first);
    for (auto it1 = clist.begin(); it1 != clist.end(); ++it1) {
      printf("%d", *it1);
    }
    printf("\n");
  }

  // add1Tree(ta, tb);
  printTree(ta);
  // printf("\ntb:\n");
  // printTree(tb);  
  printf("\n");
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
  TreeRef t2 = (TreeRef) malloc(treeSize(depth));
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
