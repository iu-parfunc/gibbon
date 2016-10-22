// A manual implementation of a single-buffer, packed bintree
// representation and a treewalk of it.

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef TRIALS
#define TRIALS 501
#endif

enum Tree {
  Nullterm = 0,
  Leaf = 1,
  Node = 2,
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
  *res = 0; // Null terminate.
  printf("wrote %d\n", (int)(res - buf));
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
  while(1) {
    char tag = *t;
    // Zero terminate the string:
    if (tag == 0) break;
    if (tag == Leaf) {
      *tout = Leaf;
      *(Num*)tout = (*(Num*)t) + 1;
      t    += 1 + sizeof(Num);
      tout += 1 + sizeof(Num);;
    } else {
      *tout = Node;      
      t++; tout++;
    }
  }
}

#include "treebench_packed/main_fragment.h"
