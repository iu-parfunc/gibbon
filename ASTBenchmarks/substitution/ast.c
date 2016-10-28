
#include "ast.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sexp.h>
#include <sexp_ops.h>

#define STREQ(a, b)        ((strcmp(a, b) == 0))
#define TAG(tag)           STREQ(sx->val, tag)
#define NEW_AST()          (ast_t*) calloc(1, sizeof(ast_t))
#define NEW_ASTS(n)        (ast_t**) calloc(n, sizeof(ast_t))
#define FREE_AST(ast)      free(ast)
#define NEXT(sx)           sx->next; assert(sx != NULL)
#define LIST(sx)           sx->list; assert(sx != NULL)
#define LAST(sx)           assert(sx->next == NULL) 
#define SX_VALUE(sx)       assert(sx->ty == SEXP_VALUE)
#define SX_LIST(x)         assert(sx->ty == SEXP_LIST)
#define IS_SX_VALUE(sx)    ((sx && sx->ty == SEXP_VALUE))
#define IS_SX_LIST(sx)     ((sx && sx->ty == SEXP_LIST))

typedef struct {
  ast_node_type parent;
} ctx_t;

ctx_t CTX(ast_node_type type) {
  ctx_t ctx;
  ctx.parent = type;
  return ctx;
}

// Start Diagnostics
#define ERROR(fmt, ...)\
    printf(fmt " %s %d\n", ##__VA_ARGS__, __FILE__, __LINE__);\
  exit(EXIT_FAILURE)

#define AST_TYPE(typ)   g_ast_types[typ]

const char* g_ast_types[] = {
  "PROG", "LIST", "DEFINE_VALUES", "DEFINE_SYNTAXES", "EXPRESSION", "VARREF",
  "PLAIN_LAMBDA", "CASE_LAMBDA", "IF", "BEGIN", "BEGIN0", "LET_VALUES", 
  "LETREC_VALUES", "SET", "QUOTE", "QUOTE_SYNTAX", "WITH_CONTINUATION_MARK",
  "PLAIN_APP", "TOP", "VARIABLE_REFERENCE", "F1", "F2", "F3"
};
// End Diagnostics

char** get_strings(sexp_t* sx, int* length) {
  sexp_t* nxt = sx; 
  do {
    SX_VALUE(nxt);
    nxt = nxt->next;
    (*length)++;
  } while (nxt);

  char** syms = (char**) malloc(sizeof(char*) * (*length));
  int idx = 0;
  while (idx < *length) {
    syms[idx] = sx->val; 
    sx = sx->next;
    idx++;
  }
}

int get_list_length(sexp_t* sx) {
  int length = 0;
  while(sx) {
    length++;
    sx = sx->next;
  }
  return length;
}

ast_t* build_formals(sexp_t* sx, ctx_t ctx) {
  if (sx) {

    ast_t* ast = NEW_AST();
    if (IS_SX_VALUE(sx)) {

      LAST(sx);
      ast->ty = F3;
      ast->node.fml.sym = sx->val;
      return ast;

    } else if (IS_SX_LIST(sx)) {

      sx = LIST(sx);
      int length = 0;
      if (IS_SX_VALUE(sx)) {

        ast->node.fml.syms   = get_strings(sx, &length);
        ast->node.fml.n_syms = length;
        ast->ty = F1;
        return ast;

      } else if(IS_SX_LIST(sx)) {

        sexp_t* next = sx->next;
        if (next != NULL) {
          LAST(next);
          ast->node.fml.sym = next->val;
        }

        sx = LIST(sx); 

        ast->node.fml.syms   = get_strings(sx, &length);
        ast->node.fml.n_syms = length;
        ast->ty = F2;
        return ast;
      }
      ERROR("[AST] Error building formals inside %s", AST_TYPE(ctx.parent));
    }
    ERROR("[AST] Error building formals inside %s", AST_TYPE(ctx.parent));
  } 
  ERROR("[AST] Error building formals inside %s", AST_TYPE(ctx.parent));
}

ast_t* build_expr(sexp_t* sx, ctx_t ctx) {
  if (sx) {

    ast_t* ast = NEW_AST(); 
    if (sx->ty == SEXP_VALUE) {

      LAST(sx);
      ast->ty = VARREF;
      ast->node.data.val = sx->val;
      return ast;

    } else if (sx->ty == SEXP_LIST) {

      LAST(sx);
      sx = LIST(sx);
      assert(sx->ty == SEXP_VALUE);
      if (TAG("%plain-lambda")) {

        ast->ty = PLAIN_LAMBDA;
        sx = NEXT(sx);
        ast->node.p_lambda.fmls = build_formals(sx, CTX(PLAIN_LAMBDA)); 
        sx = NEXT(sx);
        ast->node.p_lambda.n_exps = get_list_length(sx);
        ast->node.p_lambda.exps = NEW_ASTS(ast->node.p_lambda.n_exps);
        for (int i=0; i < ast->node.p_lambda.n_exps; i++) {
          ast->node.p_lambda.exps[i] = build_expr(sx, CTX(PLAIN_LAMBDA));
          sx = sx->next;
        }
        return ast;

      } else if (TAG("case-lambda")) {

      } else if (TAG("if")) {

      } else if (TAG("begin")) {

      } else if (TAG("being0")) {

      } else if (TAG("let-values")) {

      } else if (TAG("letrec-values")) {

      } else if (TAG("set!")) {

      } else if (TAG("quote")) {

      } else if (TAG("quote-syntax")) {

      } else if (TAG("with-continuation-mark")) {

      } else if (TAG("%plain-app")) {

      } else if (TAG("#%top")) {

      } else if (TAG("#%variable-reference")) {

      } else {
        ERROR("[AST] Error building expression inside %s..", 
            AST_TYPE(ctx.parent));
      }  
    } else {
      ERROR("[AST] Error building expression inside %s..", 
          AST_TYPE(ctx.parent));
    }
  }
}

ast_t* build_prog(sexp_t* sx) {
  ast_t* ast = NEW_AST();
  ast->node.prog.n_tops = get_list_length(sx);
  ast->node.prog.toplvl = NEW_ASTS(ast->node.prog.n_tops); 
  for (int i=0; i < ast->node.prog.n_tops; i++) {
    sexp_t* next = sx->next;
    if (IS_SX_LIST(sx)) {

      sx = LIST(sx);
      if (IS_SX_VALUE(sx)) {

        if (TAG("#%expression")) { // (#%expression expr)

          sx = NEXT(sx);
          ast->node.prog.toplvl[i] = build_expr(sx, CTX(PROG));
          
        } else if (TAG("module")) {

        } else if (TAG("#%plain-module-begin")) {

        } else if (TAG("begin")) {

        } else if (TAG("begin-for-syntax")) {

        } else if (TAG("#%provide")) {

        } else if (TAG("#%declare")) {

        } else if (TAG("module*")) {

        } else if (TAG("define-values")) {

        } else if (TAG("define-syntaxes")) {

        } else if (TAG("#%require")) {

        } else {
          ERROR("[AST] Error building top inside prog..");
        }
      } else {
        ERROR("[AST] Error building top inside prog..");
      }
    } else if (IS_SX_VALUE(sx)) {

     ast_t* chld = NEW_AST();
     chld->ty = VARREF;
     chld->node.data.val = sx->val;
     ast->node.prog.toplvl[i] = chld; 

    }
    sx = sx->next;
  }

  return ast;
}

ast_t* build_ast(void* parse_tree) {
  sexp_t* sx = (sexp_t*) parse_tree;
  return build_prog(sx);
}
