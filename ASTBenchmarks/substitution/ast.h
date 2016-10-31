
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

#include <stdio.h>
#include <stdbool.h>

#define AST struct exp 
#define LIST_DELIM '|'

typedef enum {
  MKPROG,
  DEFINE_VALUES,
  DEFINE_SYNTAXES,
  BEGINTOP,
  EXPRESSION,
  VARREF,
  LAMBDA,
  CASE_LAMBDA,
  MKLAMBDACASE,
  IF,
  BEGIN,
  BEGIN0,
  LET_VALUES,
  MKLVBIND,
  LETREC_VALUES,
  SETBANG,
  QUOTE,
  QUOTE_SYNTAX,
  QUOTE_SYNTAX_LOCAL,
  INTLIT,
  WITH_CONTINUATION_MARK,
  APP,
  TOP,
  VARIABLE_REFERENCE,
  VARIABLE_REFERENCE_TOP,
  VARIABLE_REFERENCE_NULL,
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
      int n_syms;
      char** syms;
      AST* exp;
    } defs;

    struct {
      AST* exp;
    } expr;

    struct {
      char* val;
    } data;  // QUOTE, QUOTE_SYNTAX, QUOTE_SYNTAX, TOP, VARREF, VARIABLE_REFERENCE, F3

    struct {
      int data;
    } quote;

    struct {
      AST* fmls;
      int n_exps;
      AST** exps;
    } lambda;

    struct {
      int n_lams;
      AST** lams;
    } c_lambda;

    struct {
      AST* fmls;
      int n_exps;
      AST** exps;
    } mk_lambda;

    struct {
      AST* cond;
      AST* if_e;
      AST* else_e;
    } iff; 

    struct {
      int n_exps;
      AST** exps;
    } begin;

    struct {
      AST* exp;
      int n_exps;
      AST** exps;
    } begin0;

    struct {
      bool letrec;
      int n_binds;
      AST** binds;
      int n_exps;
      AST** exps; 
    } let;

    struct {
      int n_syms;
      char** syms;
      AST* exp;       
    } binder;

    struct {
      char* sym; 
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
void print_ast(ast_t* ast, FILE* fp);

#endif /* _AST_H_ */
