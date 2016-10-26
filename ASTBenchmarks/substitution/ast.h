
// build_ast  :: parse tree -> ast  
// pack_ast   :: ast -> packed ast 
// substitute :: packed ast -> packed ast

#ifdef MULTI_BUFFER // Future multi buffer representation
typedef struct { 
  char* buf_start; 
  int index; 
  int offset; 
} buf_idx;

#define AST buf_idx
#define get_flat_addr(x) (x->buf_start + index + offset);

#else
#define AST exp 
#define get_flat_addr(x) 
#endif

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

typedef struct exp {
  ast_node_type type;
  union {
    char* val; 

    struct {  
      char* id;
    } top_exp;  

    struct {
      char* id;
      bool top;
      bool leaf;
    } var_ref_exp;

    struct {
      // Immutable for substituion pass. So we can inline within node.
      char* val;            // Inlineable 
    } quote_exp;

    struct {
      char* val;            // Inlineable
    } quote_syntax_exp;

    struct {
      uint8_t num_formals;
      char** formals;       // Inlineable 
      uint8_t num_bodies;
      AST** bodies;
    } lambda_exp;

    struct {
      uint8_t num_lambdas;
      AST** lambdas;
    } case_lambda_exp;

    struct {
      uint8_t num_binders;
      uint8_t num_bodies;
      AST** binders;        // Inlineable
      AST** bodies;         // Inlineable
    } let_exp;

    struct {
      uint8_t num_binders;
      char** binders;       // Inlineable
      AST* body;
    } binder_exp;

    struct {
      AST* cond;
      AST* if_body;
      AST* else_body; 
    } if_exp;

    struct {
      uint8_t num_bodies;
      AST** bodies;
    } begin_exp;

    struct {
      uint8_t num_bodies;
      AST* body;
      AST** bodies;
    } begin0_exp;

    struct {
      uint8_t num_bodies;
      AST** bodies;
    } app_exp;

    struct {
      char* id;            // Inlineable??
      AST* val;
    } set_exp;

    struct {
      AST* key;
      AST* val;
      AST* result;
    } cont_exp;

    struct {
      AST* expr;
    } expr_exp;

    struct {
      uint8_t num_vals;
      char** vals;         // Inlineable
      AST* expr;
    } define_vals_exp;

    struct {
      uint8_t num_syntaxes;
      char** syntaxes;     // Inlineable
      AST* expr;
    } define_syntaxes_exp;

    struct {
      uint8_t num_exprs;
      AST** exprs;
    } require_exp;

    struct {
      uint8_t num_exprs;
      AST** exprs;
    } provide_exp;

    struct {
      uint8_t num_exprs;
      AST** exprs;
    } begin_for_exp;

    struct {
      uint8_t num_exprs;
      AST** exprs;         
    } declare_exp;

    struct {
      char* id;            // Inlineable
      char* path_or_false; // Inlineable
      uint8_t num_exprs;
      AST** exprs;         
    } module_exp;
  } node;
} ast_t;

/*
// AST List
typedef struct ast_list_node_t {
  ast_t* ast;
  struct ast_list_node_t* next;
} ast_list_node_t;

typedef struct ast_list_t {
  ast_list_node_t* head;
  int length;
} ast_list_t; 

ast_list_t* ast_list_append(ast_list_t* l1, ast_list_t* l2);
void ast_list_append_node(ast_t* ast, ast_list_t* l);
void ast_list_prepend_node(ast_t* ast, ast_list_t* l);
ast_t* create_ast_node(ast_node_type type);
ast_node_type get_node_type(char* val);

void print_ast(ast_list_t* ast_list);
void print_ast_list(ast_list_t* l);
void print_ast_node_type(ast_node_type type);
void print_ast_node(ast_t* node);
*/
