
#include "subst.h"
#include <string.h>
#include <stdlib.h>

void substitute_node(char** const in, char** const out, const char* const from,
    const char* const to);
void substitute_nodes(char** const in, char** const out, 
    const char* const from, const char* const to);

char* find_sym(char* str, char* syms) {

}

void substitute_nodes(char** const in, char** const out, 
    const char* const from, const char* const to) {
  int n_asts    = *(int*) *in;
  *(int*) *out += n_asts;
  *in          += sizeof(int);
  *out         += sizeof(int);
  for (int i=0; i < n_asts; i++) {
    size_t chld_sz  = *(size_t*) *in;
    *(size_t*) *out = chld_sz;
    *out += sizeof(size_t);
    *in  += sizeof(size_t);
    substitute_node(in, out, from, to);
  }
}

void substitute_node(char** const in, char** const out, const char* const from,
    const char* const to) {
  switch(**in) {
    case BEGINTOP:
      substitute_nodes(in, out, from, to);
      return;
  }
}

void substitute(char* in, char* from, char* to) {
  if (in) {
    // Get the symbol table and tree size
    size_t sym_tbl_sz = *(size_t*) in;
    char* syms        = in + sizeof(size_t);
    in               += sizeof(size_t) + sym_tbl_sz;
    size_t tree_sz    = *(size_t*) in;

    // Allocate memory
    char* sym         = find_sym(syms, to); 
    size_t len        = strlen(to);
    char* out;
    if (sym) {
      out = (char*) calloc(1, tree_sz + sym_tbl_sz + 2 * sizeof(size_t));
    } else {
      out = (char*) calloc(1, tree_sz + sym_tbl_sz + 2 * sizeof(size_t) + 
          + sizeof(size_t) + len); 
    }

    // Populate the new symbol table
    memcpy(out, in, sizeof(size_t) + sym_tbl_sz);         // Copy symbol table
    *(size_t*) out = sym_tbl_sz + sizeof(size_t) + len;   // Write new symbol table size
    out           += sizeof(size_t) + sym_tbl_sz;         // Move to the end of the old table
    *(size_t*) out = len;                                 // Write the new string
    out           += sizeof(size_t);
    memcpy(out, to, len);
    out           += len;

    // Populate the new tree
    *(size_t*) out = *(size_t*) in;                       // Copy tree size
    out += sizeof(size_t);
    in  += sizeof(size_t);

    substitute_node(&in, &out, from, to);
  }
}
