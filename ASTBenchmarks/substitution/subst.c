
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sexp.h>
#include <sexp_ops.h>

typedef enum {
  LET,
  IF,
  BEGIN,
  BEGIN0,
  SET,
  WITH_CONTINUATION_MARK,
  LAMBDA,
  CASE_LAMBDA,
  TOP,
  VARIABLE_REFERENCE,
  SYMBOL,
  MODULE_BEGIN,
  APP,
  MODULE,
  PROVIDE,
  BEGIN_FOR_SYNTAX,
  EXPRESSION,
  DECLARE,
  OTHER,
  QUOTE,
  QUOTE_SYNTAX,
  DEFINE_VALUES,
  DEFINE_SYNTAXES,
  REQUIRE
} ast_node_type;

typedef struct ast_list_t ast_list_t;

typedef struct {
  ast_node_type type;
  ast_list_t* children;
} ast_t;

// AST List
typedef struct ast_list_node_t {
  ast_t* ast;
  struct ast_list_node_t* next;
} ast_list_node_t;

typedef struct ast_list_t {
  ast_list_node_t* head;
  int length;
} ast_list_t; 

void ast_list_append(ast_list_t* l1, ast_list_t* l2);
void ast_list_append_node(ast_t* ast, ast_list_t* l);
void ast_list_prepend_node(ast_t* ast, ast_list_t* l);

void ast_list_append(ast_list_t* l1, ast_list_t* l2) {
  ast_list_node_t* head = l2->head;

  while (head) {
    ast_list_append_node(head->ast, l1);
    head = head->next;
  }
}

void ast_list_append_node(ast_t* ast, ast_list_t* l){
  ast_list_node_t* head = l->head;
  ast_list_node_t* prev;

  while (head) {
    prev = head;
    head = head->next;
  }

  if (prev) {
    ast_list_node_t* node = (ast_list_node_t*) malloc(sizeof(ast_list_node_t));
    node->ast = ast;
    prev->next = node;
    l->length++;
  }
}

void ast_list_prepend_node(ast_t* ast, ast_list_t* l){
  ast_list_node_t* prev_head = l->head;
  ast_list_node_t* node = (ast_list_node_t*) malloc(sizeof(ast_list_node_t));
  node->ast = ast;
  node->next = prev_head;
  l->head = node;
}

ast_t* create_ast_node(ast_node_type type) {
  ast_t* node = (ast_t*) malloc(sizeof(ast_t));
  node->type = type;
  node->children = NULL;
  return node;
}

ast_node_type get_node_type(char* val) {
  if (strcmp(val, "#%top") == 0) {
    return TOP;
  } else if (strcmp(val, "#%variable-reference") == 0) {
    return VARIABLE_REFERENCE;
  } else if (strcmp(val, "quote") == 0) {
    return QUOTE;
  } else if (strcmp(val, "quote-syntax") == 0) {
    return QUOTE_SYNTAX;
  } else if (strcmp(val, "lambda") == 0 || strcmp(val, "#%plain-lambda") == 0) {
    return LAMBDA;
  } else if (strcmp(val, "#%app") == 0 || strcmp(val, "#%plain-app") == 0) {
    return APP;
  } else if (strcmp(val, "module") == 0 || strcmp(val, "module*") == 0) {
    return MODULE;
  } else if (strcmp(val, "let-values") == 0 || 
      strcmp(val, "letrec-values") == 0 || strcmp(val, "let") == 0) {
    return LET;
  } else if (strcmp(val, "#%expression") == 0) {
    return EXPRESSION;
  } else if (strcmp(val, "begin") == 0) {
    return BEGIN;
  } else if (strcmp(val, "begin-for-syntax") == 0) {
    return BEGIN_FOR_SYNTAX;
  } else if (strcmp(val, "#%provide") == 0) {
    return PROVIDE;
  } else if (strcmp(val, "#%declare") == 0) {
    return DECLARE;
  } else if (strcmp(val, "#%top") == 0) {
    return TOP;
  } else if (strcmp(val, "case-lambda") == 0) {
    return CASE_LAMBDA;
  } else if (strcmp(val, "if") == 0) {
    return IF;
  } else if (strcmp(val, "begin") == 0) {
    return BEGIN;
  } else if (strcmp(val, "begin0") == 0) {
    return BEGIN0;
  } else if (strcmp(val, "set!") == 0) {
    return SET;
  } else if (strcmp(val, "with-continuation-mark") == 0) {
    return WITH_CONTINUATION_MARK;
  } else if (strcmp(val, "define-values") == 0) {
    return DEFINE_VALUES;
  } else if (strcmp(val, "define-syntaxes") == 0) {
    return DEFINE_SYNTAXES;
  } else if (strcmp(val, "#%require") == 0) {
    return REQUIRE;
  } else {
    return SYMBOL;
  }
}

void print_ast_node_type(ast_node_type type) {
  switch (type) {
    case LET:
      printf("LET ");
    case IF:
      printf("IF ");
    case BEGIN:
      printf("BEGIN ");
    case BEGIN0:
      printf("BEGIN0 ");
    case SET:
      printf("SET ");
    case WITH_CONTINUATION_MARK:
      printf("WITH_CONTINUATION_MARK ");
    case LAMBDA:
      printf("LAMBDA ");
    case CASE_LAMBDA:
      printf("CASE_LAMBDA ");
    case TOP:
      printf("TOP ");
    case VARIABLE_REFERENCE:
      printf("VARIABLE_REFERNCE ");
    case SYMBOL:
      printf("SYMBOL ");
    case MODULE_BEGIN:
      printf("MODULE_BEGIN ");
    case APP:
      printf("APP ");
    case MODULE:
      printf("MODULE ");
    case PROVIDE:
      printf("PROVIDE ");
    case BEGIN_FOR_SYNTAX:
      printf("BEGIN_FOR_SYNTAX ");
    case EXPRESSION:
      printf("EXPRESSION ");
    case DECLARE:
      printf("DECLARE ");
    case QUOTE:
      printf("QUOTE ");
    case QUOTE_SYNTAX:
      printf("QUOTE_SYNTAX ");
    case DEFINE_VALUES:
      printf("DEFINE_VALUES ");
    case DEFINE_SYNTAXES:
      printf("DEFINE_SYNTAXES ");
    case REQUIRE:
      printf("REQUIRE ");
    case OTHER:
      printf("SYMBOL ");
    default:
      printf("ERROR ");
      exit(1);
  }
}

void print_ast(ast_list_t* ast_list);

void print_ast_node(ast_t* node) {
  if (node->children->length) {
    printf("(");
  }

  print_ast_node_type(node->type);

  if (node->children->length) {
    print_ast(node->children);
    printf(")");
  } 
}

void print_ast(ast_list_t* ast_list) {
  ast_list_node_t* head = ast_list->head;
  while (head) {
    print_ast_node(head->ast);
  }
}

ast_list_t* build_ast(sexp_t* sx, bool header) {
  if (sx) {
    switch(sx->ty) {
      case SEXP_VALUE:
      {
        ast_node_type type; 
        if (header) {
          type = get_node_type(sx->val);
        }

        ast_t* node = create_ast_node(type);
        ast_list_t* l = build_ast(sx->next, false);
        ast_list_prepend_node(node, l);
        return l;
      }
      case SEXP_LIST:
      {
        ast_list_t* l = build_ast(sx->list, true);
        ast_list_t* next = build_ast(sx->next, true);
        ast_list_append(l, next);
        return;
      }
      default:
        return;
    }
  }

  ast_list_t* l = (ast_list_t*) malloc(sizeof(ast_list_t));
  l->head = NULL;
  l->length = 0;
  return l;
}

int main(int argc, char **argv) {
  FILE *fp;
  char *status;
  sexp_t *sx;
  // dict_t *env = NULL;

  // Read file as a string 
  fp = fopen("sexps.in","r+");

  fseek(fp, 0, SEEK_END);
  long fsize = ftell(fp);
  fseek(fp, 0, SEEK_SET);  //same as rewind(f);

  char *sexp = malloc(fsize + 1);
  fread(sexp, fsize, 1, fp);
  fclose(fp);

  sexp[fsize] = 0;

  // Generate the parse tree
  if (sexp) {
    sx = parse_sexp(sexp, fsize);
  } else {
    printf("Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }

  // Print the parse tree
  int ret = print_sexp(sexp, BUFSIZ, sx);

  if (ret == -1) {
    printf("Error printing the parse tree with error : %d\n", sexp_errno);
    exit(EXIT_FAILURE);
  }

  // printf("%s\n", sexp);

  ast_list_t* ast = build_ast(sx, true);

  print_ast(ast);

  destroy_sexp(sx);

  sexp_cleanup();

  exit(EXIT_SUCCESS);
}
