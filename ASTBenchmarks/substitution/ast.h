
#ifndef _AST_H_
#define _AST_H_

/** 
 * Defines the pointer based compressed AST for the Racket syntax at [1].
 * Compressed definition :
 *
 * (data prog [prog (list toplvl)])
 * (data toplvl
 *       [define-values (list Sym) expr]
 *       [define-syntaxes (list Sym) expr]
 *       [expression expr])
 * (data expr [VARREF Sym] [plain-lambda ...]...)
 * (data formals
 *       [F1 (list Sym)]
 *       [F2 (list Sym) Sym]
 *       [F3 Sym])
 *
 * [1] https://docs.racket-lang.org/reference/syntax-model.html#%28part._fully-expanded%29
 **/

// build_ast  :: parse tree -> ast  
// pack_ast   :: ast -> packed ast 
// substitute :: packed ast -> packed ast

#include <stdbool.h>

#define AST struct exp 
#define LIST_DELIM '|'

typedef enum {
  PROG,
  DEFINE_VALUES,
  DEFINE_SYNTAXES,
  EXPRESSION,
  VARREF,
  PLAIN_LAMBDA,
  CASE_LAMBDA,
  IF,
  BEGIN,
  BEGIN0,
  LET_VALUES,
  LETREC_VALUES,
  SET,
  QUOTE,
  QUOTE_SYNTAX,
  WITH_CONTINUATION_MARK,
  PLAIN_APP,
  TOP,
  VARIABLE_REFERENCE,
  F1,
  F2,
  F3
} ast_node_type;

typedef struct exp {
  ast_node_type ty;
  union {
    struct {
      int n_tops;
      AST** toplvl;
    } prog;       

    struct {
      char* flat;
      int nests;
      char** syms;
      AST* exp;
    } def_vals;

    struct {
      char* flat;
      int nests;
      char** syms;
      AST* exp;
    } def_sytxs;

    struct {
      char* flat;
      int nests;
      AST* exp;
    } expr;

    struct {
      char* val;
    } data;  // QUOTE, QUOTE_SYNTAX, QUOTE_SYNTAX, TOP, VARREF, VARIABLE_REFERENCE, F3

    struct {
      AST* fmls;
      int n_exps;
      AST** exps;
    } p_lambda;

    struct {
      int n_lams;
      AST** lams;
    } c_lambda;

    struct {
      AST* cond;
      AST* if_e;
      AST* else_e;
    } iff; 

    struct {
      bool begin0;
      int n_exps;
      AST** exps;
    } begin;

    struct {
      bool letrec;
      int n_binds;
      AST** binds;
      int n_exps;
      AST** exps; 
    } let;

    struct {
      AST* fml;
      AST* exp;       
    } binder;

    struct {
      AST* fml; 
      AST* exp;
    } set;

    struct {
      AST* key;
      AST* val;
      AST* res;
    } wcm;

    struct {
      int n_exps;
      AST** exps;
    } app;

    struct {
      int n_syms;
      char** syms;
      char* sym;
    } fml;
  } node;
} ast_t;

// AST operations
ast_t* build_ast(void* parse_tree);

#endif /* _AST_H_ */
