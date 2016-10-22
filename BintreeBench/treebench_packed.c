// A manual implementation of a single-buffer, packed bintree
// representation and a treewalk of it.

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef TRIALS
#define TRIALS 301
#endif

enum Tree {
    Leaf,
    Node,
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
  int bytes  = (sizeof(Num)*leaves + sizeof(char)*(nodes+leaves));
  /* printf("treeSize(%d): %d bytes (%d/%d nodes/leaves)\n", */
  /*        n, bytes, nodes, leaves); */
  return bytes;
}

TreeRef buildTree(int n) {
  int bytes = treeSize(n);
  char* buf = malloc(bytes);
  char* res = fillTree(buf, n, 1);
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

#include "treebench_packed/main_fragment.h"
