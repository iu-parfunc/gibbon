
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "ast.h"

// AST List operations 
ast_list_t* ast_list_append(ast_list_t* l1, ast_list_t* l2) {

  ast_list_t* l = (ast_list_t*) malloc(sizeof(ast_list_t));
  l->head = NULL;
  l->length = 0;

  ast_list_node_t* head = l1->head;

  while (head) {
    ast_list_append_node(head->ast, l);
    head = head->next;
  }

  head = l2->head;

  while (head) {
    ast_list_append_node(head->ast, l);
    head = head->next;
  }

  return l;
}

void ast_list_append_node(ast_t* ast, ast_list_t* l){
  ast_list_node_t* head = l->head;
  ast_list_node_t* prev = NULL;

  ast_list_node_t* node = (ast_list_node_t*) malloc(sizeof(ast_list_node_t));
  node->ast = ast;
  node->next = NULL;

  if (!head) {
    l->head = node;
    l->length++;
    return;
  }

  while (head) {
    prev = head;
    head = head->next;
  }

  if (prev) {
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
  l->length++;
}

// AST utils 
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

// Tracing
void print_ast_node_type(ast_node_type type) {
  switch (type) {
    case LET:
      printf("LET ");
      break;
    case IF:
      printf("IF ");
      break;
    case BEGIN:
      printf("BEGIN ");
      break;
    case BEGIN0:
      printf("BEGIN0 ");
      break;
    case SET:
      printf("SET ");
      break;
    case WITH_CONTINUATION_MARK:
      printf("WITH_CONTINUATION_MARK ");
      break;
    case LAMBDA:
      printf("LAMBDA ");
      break;
    case CASE_LAMBDA:
      printf("CASE_LAMBDA ");
      break;
    case TOP:
      printf("TOP ");
      break;
    case VARIABLE_REFERENCE:
      printf("VARIABLE_REFERNCE ");
      break;
    case SYMBOL:
      printf("SYMBOL ");
      break;
    case MODULE_BEGIN:
      printf("MODULE_BEGIN ");
      break;
    case APP:
      printf("APP ");
      break;
    case MODULE:
      printf("MODULE ");
      break;
    case PROVIDE:
      printf("PROVIDE ");
      break;
    case BEGIN_FOR_SYNTAX:
      printf("BEGIN_FOR_SYNTAX ");
      break;
    case EXPRESSION:
      printf("EXPRESSION ");
      break;
    case DECLARE:
      printf("DECLARE ");
      break;
    case QUOTE:
      printf("QUOTE ");
      break;
    case QUOTE_SYNTAX:
      printf("QUOTE_SYNTAX ");
      break;
    case DEFINE_VALUES:
      printf("DEFINE_VALUES ");
      break;
    case DEFINE_SYNTAXES:
      printf("DEFINE_SYNTAXES ");
      break;
    case REQUIRE:
      printf("REQUIRE ");
      break;
    case OTHER:
      printf("SYMBOL ");
      break;
    default:
      printf("ERROR ");
      exit(1);
  }
}

void print_ast_node(ast_t* node) {
  if (node->children && node->children->length) {
    printf("(");
  }

  print_ast_node_type(node->type);

  if (node->children && node->children->length) {
    print_ast(node->children);
    printf(")");
  } 
}

void print_ast(ast_list_t* ast_list) {
  ast_list_node_t* head = ast_list->head;
  while (head) {
    print_ast_node(head->ast);
    head = head->next;
  }
}

void print_ast_list(ast_list_t* l) {
  ast_list_node_t* head = l->head;
  
  while (head) {
    print_ast_node_type(head->ast->type);
    head = head->next;
  }
}
