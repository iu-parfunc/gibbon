// A manual implementation of a single-buffer, packed bintree
// representation and a treewalk of it.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// This was enough for a 10X slowdown.  It wrecks the add1tree recursion.
// #define cilk_spawn
// #define cilk_sync {}
#include <cilk/cilk.h>

#ifndef TRIALS
#define TRIALS 301
#endif

// The bottom K layers of the tree have NO indirections.
#define SEQLAYERS 15

enum Tree {
    Leaf,
    Node,
    // An alternative version with size info.  Leaf doesn't need it.
    NodePrime
};

// Manual layout:
// one byte for each tag, 64 bit integers
typedef char* TreeRef;
typedef long long Num;

typedef uint32_t TreeSize;

// Helper function
TreeRef fillTree(TreeRef cursor, int n, Num root) {
  // printf("  filltree: %p, n=%d, fill=%lld\n", cursor, n, root); fflush(stdout);

  if (n == 0) {
    *cursor = Leaf;
    cursor++;    
    *((Num*)cursor) = root; // Unaligned!
    // printf("; wrote tag %d, payload %lld\n", Leaf, root); fflush(stdout);
    return (cursor + sizeof(Num));
  } else if (n > SEQLAYERS) {
    *cursor = NodePrime;
    cursor++;
    TreeSize* left_size = (TreeSize*)cursor;
    cursor += sizeof(TreeSize);

    char* cur2 = fillTree(cursor, n-1, root);
    *left_size = cur2 - cursor;
    return fillTree(cur2, n-1, root + (1<<(n-1)));
    
  } else {
    *cursor = Node;
    // printf("; wrote tag %d\n", Node); fflush(stdout);
    char* cur2 = fillTree(cursor+1, n-1, root);
    return fillTree(cur2, n-1, root + (1<<(n-1)));
  }
}

size_t treeSize(int n) {
  int leaves = 1 << n;
  int nodes  = leaves - 1;
  // Both nodes and leaves are tagged:
  int bytes  = (sizeof(Num)*leaves + sizeof(char)*(nodes+leaves));
  printf("  treeSize(%d): %d bytes (%d/%d nodes/leaves)\n",
         n, bytes, nodes, leaves);

  // Double it for Prime nodes:
  return 2 * bytes;
}

TreeRef buildTree(int n) {
  size_t bytes = treeSize(n);
  char* buf = malloc(bytes);
  char* res = fillTree(buf, n, 1);
  printf("  wrote %d bytes while building tree\n", (int)(res - buf));  
  return buf;
}

TreeRef printTree(TreeRef t) {
  char tag = *t;
  if (tag == Leaf) {
    t++;
    printf("%lld", *(Num*)t);
    return (t+sizeof(Num));
  } else if (tag == Node) {    
    t++;
    printf("(");
    TreeRef t2 = printTree(t);
    printf(",");
    TreeRef t3 = printTree(t2);
    printf(")");
    return t3;
  } else { // NodePrime
    t += 1 + sizeof(TreeSize);
    printf("{");
    TreeRef t2 = printTree(t);
    printf(",");
    TreeRef t3 = printTree(t2);
    printf("}");
    return t3;    
  }
}


TreeRef add1Seq(TreeRef t, TreeRef tout) {
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
        
    TreeRef t3    = add1Seq(t2, tout2);
    TreeRef tout3 = tout2 + (t3 - t2);
    return add1Seq(t3, tout3);
  }
}

/* add1tree, in parallel

   The first version of this yielded a slowdown based on some
   non-obvious aspect of the control flow.  A significant (but)
   smaller amount of slowdown could be demonstrated just by adding a
   single extra IF test to the original treebench_packed.c:

     if (tag == Leaf) {
     } else
       // Creating a second IF test here yields a LARGE slowdown.
       if (tag == Node)
     {
     }
     // But the slowdown from the if is worst if we LACK the final case.
     // Otherwise it's a small slowdown, like 2.6 to 2.8ms.
     else return 0;

 */
TreeRef add1Tree(TreeRef t, TreeRef tout) {
  if ( *t == NodePrime ) {
    // printf("add1Tree: got NodePrime..\n");
    *tout = NodePrime;
    TreeSize left_sz = *(TreeSize*)(t+1);
    
    TreeRef t2    = t    + 1 + sizeof(TreeSize);
    TreeRef tout2 = tout + 1 + sizeof(TreeSize);

    // We can spawn the left recursion and we don't need the end
    // cursor.  We already know what it will be:
    cilk_spawn add1Tree(t2, tout2);
    
    TreeRef t3    = t2    + left_sz;
    TreeRef tout3 = tout2 + left_sz;
    TreeRef t4    = add1Tree(t3, tout3);
    cilk_sync;
    return t4;
  }
  else {
    // printf("add1Tree: bottoming out to add1Seq\n");
    return add1Seq(t,tout);
  }
}

#include "treebench_packed/main_fragment.h"
