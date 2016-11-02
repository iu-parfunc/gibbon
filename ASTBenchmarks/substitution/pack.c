
#include "pack.h"
#include "debug.h"
#include "uthash.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define ADD_TREE_SZ(a, b) \
    a.n_syms     += b.n_syms;\
    a.sym_tbl_sz += b.sym_tbl_sz;\
    a.tree_sz    += b.tree_sz;
#define WRITE_STRING(out, str) \
    *(intptr_t*) out = (intptr_t) str; \
    out += sizeof(intptr_t);


typedef struct {
  int n_syms;
  size_t sym_tbl_sz;
  size_t tree_sz;
} tree_sz_t;

struct sym_hash {
  char* sym;                     // Hash key
  int count;                     // Hash value. Not important
  UT_hash_handle hh;
};

struct sym_addr_hash {
  char* sym;                     // Hash key
  char* addr;                    // Hash value. Interned string address
  UT_hash_handle hh;
};

struct sym_hash* g_pack_syms = NULL;
struct sym_addr_hash* g_pack_sym_addrs = NULL;

void add_symbol(char* sym, tree_sz_t* size) {
  struct sym_hash* s;
  HASH_FIND_STR(g_pack_syms, sym, s);
  if (!s) {
    size->sym_tbl_sz += strlen(sym);
    size->sym_tbl_sz += sizeof(size_t);                         // Next string offset
    size->n_syms++;
    s = (struct sym_hash*) malloc(sizeof(struct sym_hash)); 
    s->sym            = sym; 
    s->count          = 1;
    HASH_ADD_KEYPTR(hh, g_pack_syms, s->sym, strlen(s->sym), s);
  }
  size->tree_sz      += sizeof(char*);                          // String pointer
}

tree_sz_t size_tree(ast_t* ast) {
  tree_sz_t size = {0};
  if (ast) {
    size.tree_sz++;
    size.tree_sz    += sizeof(size_t);                         // Next child pointer
    switch (ast->ty) {
      case BEGINTOP:
      {
        size.tree_sz      += sizeof(int);                       // # tops
        for (int i=0; i < ast->node.prog.n_tops; i++) {
          tree_sz_t sz     = size_tree(ast->node.prog.toplvl[i]);
          // size.tree_sz    += sizeof(size_t);                 // Next child pointer
          ADD_TREE_SZ(size, sz);
        }
        return size;
      }
      case DEFINE_VALUES:
      case DEFINE_SYNTAXES:
      {
        for (int i=0; i < ast->node.defs.n_syms; i++) {
          add_symbol(ast->node.defs.syms[i], &size);
        }
        size.tree_sz += sizeof(int);                             // Inlined n_syms
        tree_sz_t sz  = size_tree(ast->node.defs.exp);
        ADD_TREE_SZ(size, sz);
        return size;
      }
      case EXPRESSION:
      {
        tree_sz_t sz = size_tree(ast->node.expr.exp);
        ADD_TREE_SZ(size, sz);
        return size;
      }
      case VARREF:
      {
        add_symbol(ast->node.data.val, &size);
        return size;
      }
      case LAMBDA:
      {
        tree_sz_t sz    = size_tree(ast->node.lambda.fmls);
        ADD_TREE_SZ(size, sz);

        size.tree_sz   += sizeof(int);                           // # exps
        for (int i=0; i < ast->node.lambda.n_exps; i++) {
          tree_sz_t sz  = size_tree(ast->node.lambda.exps[i]);
          // size.tree_sz += sizeof(size_t);                        // Next child pointer
          ADD_TREE_SZ(size, sz);
        }
        return size;
      }
      case CASE_LAMBDA:
        size.tree_sz   += sizeof(int);                           // # lams
        for (int i=0; i < ast->node.c_lambda.n_lams; i++) {
          tree_sz_t sz  = size_tree(ast->node.c_lambda.lams[i]);
          // size.tree_sz += sizeof(size_t);                        // Next child pointer
          ADD_TREE_SZ(size, sz);
        }
        return size;
      case MKLAMBDACASE:
      {
        tree_sz_t sz       = size_tree(ast->node.mk_lambda.fmls); 
        ADD_TREE_SZ(size, sz);
        size.tree_sz       += sizeof(int);                      // # exps
        for (int i=0; i < ast->node.mk_lambda.n_exps; i++)  {
          tree_sz_t sz     = size_tree(ast->node.mk_lambda.exps[i]);
          // size.tree_sz    += sizeof(size_t);                    // Next child pointer
          ADD_TREE_SZ(size, sz);
        }
        return size;
      }
      case F1:
      {
        for (int i=0; i < ast->node.fml.n_syms; i++) {
          add_symbol(ast->node.fml.syms[i], &size);
        }
        size.tree_sz    += sizeof(int);                         // Inlined n_syms 
        return size;
      }
      case F2:
      {
        for (int i=0; i < ast->node.fml.n_syms; i++) {
          add_symbol(ast->node.fml.syms[i], &size);
        }
        size.tree_sz    += sizeof(int);                         // Inlined n_syms
        add_symbol(ast->node.fml.sym, &size);
        return size;
      }
      case F3:
      {
        add_symbol(ast->node.fml.sym, &size);
        return size;
      }
      case IF:
      {
        tree_sz_t cond   = size_tree(ast->node.iff.cond);
        tree_sz_t if_e   = size_tree(ast->node.iff.if_e);
        tree_sz_t else_e = size_tree(ast->node.iff.else_e);
        ADD_TREE_SZ(size, cond);
        ADD_TREE_SZ(size, if_e);
        ADD_TREE_SZ(size, else_e);
        // size.tree_sz    += sizeof(size_t) * 3;                  // Next child pointers
        return size;
      }
      case BEGIN:
        size.tree_sz      += sizeof(int);                       // # exps
        for (int i=0; i < ast->node.begin.n_exps; i++) {
          tree_sz_t sz     = size_tree(ast->node.begin.exps[i]);
          ADD_TREE_SZ(size, sz);
          // size.tree_sz    += sizeof(size_t);                   // Next child pointer
        }
        return size;
      case BEGIN0:
      {
        size.tree_sz      += sizeof(size_t);                    // Next child pointer
        tree_sz_t sz       = size_tree(ast->node.begin0.exp);
        ADD_TREE_SZ(size, sz);
        size.tree_sz      += sizeof(int);                       // # exps
        for (int i=0; i < ast->node.begin0.n_exps; i++) {
          tree_sz_t sz     = size_tree(ast->node.begin0.exps[i]);
          ADD_TREE_SZ(size, sz);
          // size.tree_sz    += sizeof(size_t);                    // Next child pointer
        }
        return size;
      }
      case LET_VALUES:
      case LETREC_VALUES:
      {
        size.tree_sz      += sizeof(int);                       // # binds
        for (int i=0; i < ast->node.let.n_binds; i++) {
          tree_sz_t sz     = size_tree(ast->node.let.binds[i]);
          ADD_TREE_SZ(size, sz);
          // size.tree_sz    += sizeof(size_t);                     // Next child pointer
        }
        size.tree_sz      += sizeof(int);                       // # exps
        for (int i=0; i < ast->node.let.n_exps; i++) {
          tree_sz_t sz     = size_tree(ast->node.let.exps[i]);
          ADD_TREE_SZ(size, sz);
          // size.tree_sz    += sizeof(size_t);                     // Next child pointer
        }
        return size;
      }
      case MKLVBIND:
      {
        for (int i=0; i < ast->node.binder.n_syms; i++) {
          add_symbol(ast->node.binder.syms[i], &size);
        }
        size.tree_sz        += sizeof(int);                      // Inlined n_syms 
        tree_sz_t sz         = size_tree(ast->node.binder.exp);
        ADD_TREE_SZ(size, sz);
        return size;
      }
      case SETBANG:
      {
        add_symbol(ast->node.set.sym, &size);
        tree_sz_t sz = size_tree(ast->node.set.exp);
        ADD_TREE_SZ(size, sz);
        return size;
      }
      case QUOTE:
      case QUOTE_SYNTAX:
      case QUOTE_SYNTAX_LOCAL:
      {
        size.tree_sz += sizeof(long);
        return size;
      }
      case WITH_CONTINUATION_MARK:
      {
        tree_sz_t key = size_tree(ast->node.wcm.key);
        tree_sz_t val = size_tree(ast->node.wcm.val);
        tree_sz_t res = size_tree(ast->node.wcm.res);
        ADD_TREE_SZ(size, key);
        ADD_TREE_SZ(size, val);
        ADD_TREE_SZ(size, res);
        // size.tree_sz    += sizeof(size_t) * 3;               // Next child pointer
        return size;
      }
      case APP:
      {
        size.tree_sz      += sizeof(int);                     // # exps
        for (int i=0; i < ast->node.app.n_exps; i++) {
          tree_sz_t sz     = size_tree(ast->node.app.exps[i]);
          ADD_TREE_SZ(size, sz);
          // size.tree_sz    += sizeof(size_t);                 // Next child pointer
        }
        return size;
      }
      case TOP:
      case VARIABLE_REFERENCE:
      case VARIABLE_REFERENCE_TOP:
        add_symbol(ast->node.data.val, &size);
        return size;
      case VARIABLE_REFERENCE_NULL:
        return size;
      default:
        ERROR("Invalid AST node..");
    }
  }
}

size_t pack_node(ast_t* ast, char** const out_pt, char** const syms_pt);
size_t pack_nodes(ast_t** asts, int n_asts, char** const out_pt, 
    char** const syms_pt);
char* intern_string(const char* const str, char** const syms); 
char* intern_string(const char* const str, char** const syms_pt) {
  char* syms = *syms_pt;
  struct sym_addr_hash* s;
  HASH_FIND_STR(g_pack_sym_addrs, str, s);
  if (!s) {
    size_t len = strlen(str);
    *(size_t*) syms = len;
    *syms_pt += sizeof(size_t);
    memcpy(*syms_pt, str, len);

    s         = (struct sym_addr_hash*)malloc(sizeof(struct sym_addr_hash)); 
    s->sym    = (char*) str; 
    s->addr   = (char*) *syms_pt;
    *syms_pt += len;          
    HASH_ADD_KEYPTR(hh, g_pack_sym_addrs, s->sym, strlen(s->sym), s);
    return s->addr;
  } else {
    return s->addr;
  }
  ERROR("[PACK] Error while interning string %s..", str);
}

size_t pack_strings(char** strs, int n_strs, char** const out_pt,
    char** const syms_pt) {
  char* start = *out_pt;          // Start of the node

  *(int*) start = n_strs; 
  *out_pt      += sizeof(int);
  for (int i=0; i < n_strs; i++) {
    char* sym   = intern_string(strs[i], syms_pt);
    WRITE_STRING(*out_pt, sym);
  }
  return (size_t) (*out_pt - start);
}

size_t pack_nodes(ast_t** asts, int n_asts, char** const out_pt, 
    char** const syms_pt) {
  char* start       = *out_pt;          // Start of the node

  *(int*) start     = n_asts;
  *out_pt          += sizeof(int);
  for (int i=0; i < n_asts; i++) {
    // size_t* chld_sz = (size_t*) *out_pt;
    // *out_pt        += sizeof(size_t);
    size_t sz       = pack_node(asts[i], out_pt, syms_pt);
    // *chld_sz        = sz;
  }
  return (size_t) (*out_pt - start);
}

size_t pack_node(ast_t* ast, char** const out_pt, char** const syms_pt) {

  char* start = *out_pt;        // Start of the node
  **out_pt    = (char) ast->ty; // We know the enum is not more than range of char
  *out_pt    += 1;
  size_t* sz  = (size_t*) *out_pt;
  *out_pt    += sizeof(size_t);

  switch(ast->ty) {
    case DEFINE_VALUES:
    case DEFINE_SYNTAXES:
    {
      pack_strings(ast->node.defs.syms, ast->node.defs.n_syms, out_pt, syms_pt);
      pack_node(ast->node.defs.exp, out_pt, syms_pt);
      break;
    }
    case BEGINTOP:
      pack_nodes(ast->node.prog.toplvl, ast->node.prog.n_tops, out_pt,
          syms_pt);
      break;
    case EXPRESSION:
      pack_node(ast->node.expr.exp, out_pt, syms_pt);
      break;
    case VARREF:
    {
      char* sym = intern_string(ast->node.data.val, syms_pt);
      WRITE_STRING(*out_pt, sym);
      break;
    }
    case LAMBDA:
      pack_node(ast->node.lambda.fmls, out_pt, syms_pt);
      pack_nodes(ast->node.lambda.exps, ast->node.lambda.n_exps, out_pt, 
          syms_pt);
      break;
    case CASE_LAMBDA:
      pack_nodes(ast->node.c_lambda.lams, ast->node.c_lambda.n_lams, out_pt, 
          syms_pt);
      break;
    case MKLAMBDACASE:
      pack_node(ast->node.mk_lambda.fmls, out_pt, syms_pt);
      pack_nodes(ast->node.mk_lambda.exps, ast->node.mk_lambda.n_exps, out_pt,
          syms_pt);
      break;
    case F1:
      pack_strings(ast->node.fml.syms, ast->node.fml.n_syms, out_pt, syms_pt);
      break;
    case F2:
    {
      pack_strings(ast->node.fml.syms, ast->node.fml.n_syms, out_pt, syms_pt);
      char* sym = intern_string(ast->node.fml.sym, syms_pt);
      WRITE_STRING(*out_pt, sym);
      break;
    }
    case F3:
    {
      char* sym = intern_string(ast->node.fml.sym, syms_pt);
      WRITE_STRING(*out_pt, sym);
      break;
    }
    case IF:
      pack_node(ast->node.iff.cond, out_pt, syms_pt);
      pack_node(ast->node.iff.if_e, out_pt, syms_pt);
      pack_node(ast->node.iff.else_e, out_pt, syms_pt);
      break;
    case BEGIN:
      pack_nodes(ast->node.begin.exps, ast->node.begin.n_exps, out_pt, 
          syms_pt);
      break;
    case BEGIN0:
    {
      size_t* nxt = (size_t*) *out_pt;
      *out_pt += sizeof(size_t);
      size_t sz = pack_node(ast->node.begin0.exp, out_pt, syms_pt);
      *nxt = sz;
      pack_nodes(ast->node.begin0.exps, ast->node.begin0.n_exps, out_pt, 
          syms_pt);
      break;
    }
    case LET_VALUES:
    case LETREC_VALUES:
      pack_nodes(ast->node.let.binds, ast->node.let.n_binds, out_pt, syms_pt);
      pack_nodes(ast->node.let.exps, ast->node.let.n_exps, out_pt, syms_pt);
      break;
    case MKLVBIND:
      pack_strings(ast->node.binder.syms, ast->node.binder.n_syms, out_pt,
          syms_pt);
      pack_node(ast->node.binder.exp, out_pt, syms_pt);
      break;
    case SETBANG:
    {
      char* sym = intern_string(ast->node.set.sym, syms_pt);
      WRITE_STRING(*out_pt, sym);
      pack_node(ast->node.set.exp, out_pt, syms_pt);
      break;
    }
    case QUOTE:
    case QUOTE_SYNTAX:
    case QUOTE_SYNTAX_LOCAL:
    {
      *(long*) *out_pt = ast->node.quote.data;
      (*out_pt)      += sizeof(long);
      break;
    }
    case WITH_CONTINUATION_MARK:
      pack_node(ast->node.wcm.key, out_pt, syms_pt);
      pack_node(ast->node.wcm.val, out_pt, syms_pt);
      pack_node(ast->node.wcm.res, out_pt, syms_pt);
      break;
    case APP:
      pack_nodes(ast->node.app.exps, ast->node.app.n_exps, out_pt, syms_pt);
      break;
    case TOP:
    case VARIABLE_REFERENCE:
    case VARIABLE_REFERENCE_TOP:
    {
      char* sym = intern_string(ast->node.data.val, syms_pt);
      WRITE_STRING(*out_pt, sym);
      break;
    }
    case VARIABLE_REFERENCE_NULL:
      break;
    default:
      ERROR("[PACK] Invalid AST node..");
  }
  *sz =  (size_t) (*out_pt - start);
  return *sz;
}

char* pack_ast(ast_t* ast) {
  if (ast) {

    // Get the symbol table and the tree size doing a pass over the AST
    tree_sz_t sz   = size_tree(ast);
    printf("Number of Symbols : %d\n", sz.n_syms);
    printf("Symbol table size : %lu\n", sz.sym_tbl_sz);
    printf("Tree size         : %lu\n", sz.tree_sz);

    /* free the hash table contents */
    struct sym_hash *s, *tmp;
    HASH_ITER(hh, g_pack_syms, s, tmp) {
      HASH_DEL(g_pack_syms, s);
      free(s);
    }

    // Two additional sizeof(size_t) are for size tags at the head of the 
    // symbol table and the tree
    size_t alloc_sz = sz.tree_sz + sz.sym_tbl_sz + sizeof(size_t) * 2;
    char* out = (char*) calloc(1, alloc_sz);
    // Reserve the space for symbol table at the head
    char* start = out;
    char* syms = out;
    *(size_t*) syms = (size_t) sz.sym_tbl_sz; // Symbol table size
    syms += sizeof(size_t);
    out  += sizeof(size_t);
    out  += sz.sym_tbl_sz;

    // Start writing the tree
    *(size_t*) out  = (size_t) sz.tree_sz;    // Tree size
    out += sizeof(size_t);
    pack_node(ast, &out, &syms);

    printf("Allocated size    : %lu\n", alloc_sz);
    printf("Packed            : %lu\n", (size_t)(out - start));
    assert(alloc_sz == (out - start));
    return syms;
  }
  return NULL;
}
