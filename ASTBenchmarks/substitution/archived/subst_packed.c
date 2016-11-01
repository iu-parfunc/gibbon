
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <sexp.h>
#include <sexp_ops.h>

enum Tree {
    Leaf,
    Node,
    // An alternative version with size info.  Leaf doesn't need it.
    NodePrime
};

// The top K layers of the tree have NO indirections.
#define PARLAYERS 4
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

typedef char* TreeRef;
typedef long long Num;

typedef struct {
  int nodes;
  size_t bytes;
} TreeSize;

TreeRef fillTree(TreeRef cursor, sexp_t* sx, int n) {
  if (sx) {
    switch(sx->ty) {
      case SEXP_VALUE:
        if (n < PARLAYERS) {
          // Packed tree node tag
          *cursor = (sx->next != NULL) ? NodePrime : Leaf;
          cursor++;

          // Copy the parse tree node
          sexp_t* parse_tree_node = (sexp_t*) cursor;
          memcpy(cursor, sx , sizeof(sexp_t));
          cursor += sizeof(sexp_t);

          // Fixup parse tree node pointers to point to inlined data
          parse_tree_node->val = (char*) cursor;
          parse_tree_node->val_allocated = sx->val_used;

          // Inline node data
          memcpy(cursor, sx->val, sx->val_used); 
          cursor += sx->val_used;
          size_t* child_size = (size_t*)cursor;
          cursor += sizeof(size_t);
          TreeRef next_node = cursor;

          TreeRef cur2 = fillTree(cursor, sx->next, n+1);

          if (!sx->next) {
            parse_tree_node->next = (sexp_t*) (next_node + 1);
          }

          // Store the child tree size
          *child_size = cur2 - cursor;
          return cur2;
        } else {
          *cursor = (sx->next != NULL) ? Node : Leaf;
          cursor++;
          // Copy the parse tree node
          sexp_t* parse_tree_node = (sexp_t*) cursor;
          memcpy(cursor, sx , sizeof(sexp_t));
          cursor += sizeof(sexp_t);

          // Fixup parse tree node pointers to point to inlined data
          parse_tree_node->val = (char*) cursor;
          parse_tree_node->val_allocated = sx->val_used;

          // Inline node data
          memcpy(cursor, sx->val, sx->val_used); 
          cursor += sx->val_used;

          TreeRef next_node = cursor;

          TreeRef cur2 = fillTree(cursor, sx->next, n+1);

          if (!sx->next) {
            // Fixup parse tree node next pointer to point next packed node
            parse_tree_node->next = (sexp_t*) (next_node + 1);
          }

          return cur2;
        }
      case SEXP_LIST:
        if (n < PARLAYERS) {
          // Packed tree node tag
          *cursor = NodePrime;
          cursor++;

          // Copy the parse tree node
          sexp_t* parse_tree_node = (sexp_t*) cursor;
          memcpy(cursor, sx , sizeof(sexp_t));
          cursor += sizeof(sexp_t);

          size_t* left_size = (size_t*)cursor;
          cursor += sizeof(size_t);

          TreeRef next_node = cursor;

          // Fill the left subtree
          TreeRef left = fillTree(cursor, sx->list, n+1);

          if (!sx->list) {
            // Fixup parse tree node next pointer to left subtree 
            parse_tree_node->list = (sexp_t*) (next_node + 1);
          }

          // Store the child tree size
          *left_size = left - cursor;

          if (!sx->next) {
            // Fixup parse tree node pointer for right sub tree
            parse_tree_node->next = (sexp_t*) (left + 1);
          }

          // Fill the right sub tree
          return fillTree(left, sx->next, n+1);
        } else {
          // Packed tree node tag
          *cursor = NodePrime;
          cursor++;

          // Copy the parse tree node
          sexp_t* parse_tree_node = (sexp_t*) cursor;
          memcpy(cursor, sx , sizeof(sexp_t));
          cursor += sizeof(sexp_t);

          TreeRef next_node = cursor;

          // Fill the left subtree
          TreeRef left = fillTree(cursor, sx->list, n+1);

          if(!sx->list) {
            // Fixup parse tree node next pointer to left subtree
            parse_tree_node->list = (sexp_t*) (next_node + 1);
          }

          if (!sx->next) {
            parse_tree_node->next = (sexp_t*) (left + 1);
          }

          // Fill the right sub tree
          return fillTree(left, sx->next, n+1);
        }
      default:
        printf("Invalid parse tree..\n");
        exit(EXIT_FAILURE);
    }
  }

  return cursor;
}

TreeSize parseTreeSize(sexp_t* sx) {
  // Traverse the parse tree and count the number of nodes in it
  if (sx) {
    switch(sx->ty) {
      case SEXP_VALUE:
      {
        TreeSize treeSz = parseTreeSize(sx->next);
        treeSz.nodes += 1;
        treeSz.bytes += sizeof(sexp_t) + sx->val_used; 

        return treeSz;
      }
      case SEXP_LIST:
      {
        TreeSize treeSz1 = parseTreeSize(sx->next);
        TreeSize treeSz2 = parseTreeSize(sx->list); 
        TreeSize treeSz;
        treeSz.nodes = treeSz1.nodes + treeSz2.nodes + 1;
        treeSz.bytes = treeSz1.bytes + treeSz2.bytes + sizeof(sexp_t);

        return treeSz;
      }
      default:
        printf("Invalid parse tree..\n");
        exit(EXIT_FAILURE);
    }
  }

  TreeSize treeSz;
  treeSz.nodes = 0;
  treeSz.bytes = 0;
  return treeSz;
}

size_t packedTreeSize(sexp_t* sx) {
  TreeSize treeSz = parseTreeSize(sx);
  return (sizeof(char) + sizeof(size_t)) * treeSz.nodes +
    treeSz.bytes;
}

TreeRef buildTree(sexp_t* sx) {
  printf("  Running parallel version with PARLAYERS=%d\n", PARLAYERS);

  TreeSize sz = parseTreeSize(sx);

  printf("  Parse tree size : Nodes - %d Bytes - %d\n", sz.nodes, sz.bytes);
  size_t bytes = packedTreeSize(sx);
  char* buf = malloc(bytes);
  char* res = fillTree(buf, sx, 0);
  printf("  wrote %d bytes while building tree\n", (int)(res - buf));
  return buf;
}

sexp_t* substitute(sexp_t* sx, char* from, char* to) {
  if (sx) {
    sexp_t* sx_new = (sexp_t*) calloc(1, sizeof(sexp_t));
    memcpy(sx_new, sx, sizeof(sexp_t));

    switch(sx->ty) {
      case SEXP_VALUE:
        {

          sx_new->next = substitute(sx->next, from, to);
          if (sx->val != NULL && strcmp(sx->val, from) == 0) {
            sx_new->val = to;
          }

          return sx_new;
        }
      case SEXP_LIST:
        {
          sx_new->list = substitute(sx->list, from, to);
          sx_new->next = substitute(sx->next, from, to);
          return sx_new;

        }
      default:
        printf("Invalid parse tree..\n");
        exit(EXIT_FAILURE);
    }
  }
}

int main(int argc, char **argv) {  
  FILE *fp;
  int iterations = 1;
  char *status, *fn, *from, *to = "something-else";
  sexp_t *sx;
  double* times;

  // Read file as a string  
  if (argc > 2) {
    from = argv[1]; 
    fn = argv[2];
    iterations = atof(argv[3]);
  } else {
    from = "a"; 
    fn = "sexps.in";
  }

  times = (double*) calloc(iterations, sizeof(double));

  printf("Opening file: %s\n", fn);
  fp = fopen(fn,"r+");

  if (fp <= 0) {
    fprintf(stderr,"Error: Failed to open file.\n");
    exit(EXIT_FAILURE);
  }
  fseek(fp, 0, SEEK_END);
  long fsize = ftell(fp);
  fseek(fp, 0, SEEK_SET);  //same as rewind(f);

  char *sexp = malloc(fsize + 1);
  int ec = fread(sexp, fsize, 1, fp);
  fclose(fp);

  sexp[fsize] = 0;

  // Generate the parse tree
  if (sexp) {
    sx = parse_sexp(sexp, fsize);
  } else {
    fprintf(stderr,"Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }

  printf("Generated parse tree..\n");

  TreeRef root = buildTree(sx); 
  assert(*root == Node || *root == NodePrime || *root == Leaf);

  sx = (sexp_t*) (root + 1);

  /*
  size_t sz = packedTreeSize(sx);
  printf("Print the parse tree, BUFSIZ=%d:\n", sz);
  char *printed = malloc(fsize);
  int ret = print_sexp(printed, sz, sx);
  printf("%s\n",printed);

  if (ret == -1) {
    fprintf(stderr,"Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }
  free(printed);
  */

  struct timespec begin, end;

  sexp_t* sx_new;
  for (int i=0; i < iterations; i++) {
    clock_gettime(which_clock, &begin);
    sx_new = substitute(sx, from, to);
    clock_gettime(which_clock, &end);
    times[i] = difftimespecs(&begin, &end);

    printf("    run(%d): %lf\n", i, times[i]);
  }

  printf("Ran substitution for %d iterations\n", iterations);

  /*
  printf("Parse tree after substitution, BUFSIZ=%d:\n", fsize);
  printed = malloc(fsize);
  ret = print_sexp(printed, BUFSIZ, sx_new);
  printf("%s\n",printed);
  */

  qsort(times, iterations,  sizeof(double), compare_doubles);
  printf("  Sorted: ");
  for(int i=0; i< iterations; i++)
    printf(" %lf", times[i]);
  printf("\nMINTIME: %lf\n",    times[0]);
  printf("MEDIANTIME: %lf\n", times[iterations / 2]);
  printf("MAXTIME: %lf\n", times[iterations - 1]);
  printf("AVGTIME: %lf\n", avg(times, iterations));

  printf("\n --------------------------------------------------------------\n");

  // print_ast(ast);

  destroy_sexp(sx);

  // sexp_cleanup();

  exit(EXIT_SUCCESS);
}
