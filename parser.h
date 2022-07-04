#ifndef PARSER_H
#define PARSER_H

#include "vec.h"
#include "str.h"
#include "types.h"
#include "builtin.h"

typedef struct AstNode AstNode;
typedef struct Token Token;

enum OperatorOp {
    OPERATOR_ADD,
    OPERATOR_SUB,
    OPERATOR_BITAND,
    OPERATOR_BITOR,
    OPERATOR_BITXOR,
    OPERATOR_LOGICAL_AND,
    OPERATOR_LOGICAL_OR,
    OPERATOR_UNARY_LOGICAL_NOT,
    OPERATOR_MUL,
    OPERATOR_DIV,
    OPERATOR_MODULO,
    OPERATOR_ASSIGN,
    OPERATOR_PLUSASSIGN,
    OPERATOR_MINUSASSIGN,
    OPERATOR_MULASSIGN,
    OPERATOR_DIVASSIGN,
    OPERATOR_MODASSIGN,
    OPERATOR_LSHIFTASSIGN,
    OPERATOR_RSHIFTASSIGN,
    OPERATOR_BITANDASSIGN,
    OPERATOR_BITORASSIGN,
    OPERATOR_BITXORASSIGN,
    OPERATOR_EQ,
    OPERATOR_NEQ,
    OPERATOR_LT,
    OPERATOR_GT,
    OPERATOR_LTE,
    OPERATOR_GTE,
    OPERATOR_SHIFT_LEFT,
    OPERATOR_SHIFT_RIGHT,
    OPERATOR_ARRAY_INDEXING,
    OPERATOR_UNARY_NEG,
    OPERATOR_UNARY_ADDRESSOF,
    OPERATOR_UNARY_DEREF,
    OPERATOR_UNARY_BITNOT,
};

typedef struct AstNode {
    enum AstType {
        AST_MODULE, AST_FN, AST_FN_ARG, AST_EXPR, AST_DEF_VAR
    } type;

    const Token *start_token; /* for error reporting */
    AstNode *next_sibling; /* 0 is end */

    union {
        struct {
            AstNode *first_child;
        } module;

        struct {
            Str name;
            AstNode *first_arg;
            AstNode *body;
            TypeId type;
        } fn;

        struct {
            Str name;
            TypeId type;
        } fn_arg;

        struct {
            Str name;
            TypeId type;
            bool is_const;
            AstNode *value;
        } var_def;

        struct {
            enum ExprType {
                EXPR_LIST,  // like C comma operator, but using ;
                EXPR_IDENT,
                EXPR_LITERAL,
                EXPR_CALL,
                EXPR_ASM,
                EXPR_CAST,
                EXPR_IF_ELSE,
                EXPR_LOCAL_SCOPE,
                EXPR_LOOP,
                EXPR_GOTO,
                EXPR_RETURN,
                EXPR_MEMBER_ACCESS,
                EXPR_BUILTIN,
                // temporary frontend state. not passed to middle/backend
                EXPR_FE_OPERATOR,
            } type;

            TypeId eval_type;

            union {
                struct {
                    enum LiteralType {
                        LIT_VOID, LIT_U8, LIT_U16, LIT_I8, LIT_I16, LIT_INT_ANY /* not passed to backend */, LIT_STR, LIT_ARRAY, LIT_BOOL
                    } type;

                    union {
                        bool literal_bool;
                        int literal_int;
                        Str literal_str;
                        AstNode *literal_array_first_val;
                    };
                } literal;

                struct {
                    AstNode *val;
                } return_;

                struct {
                    bool is_continue; // otherwise is break
                    Str label;
                    AstNode *target; // resolved in second frontend pass
                } goto_;

                struct {
                    Str var_name;
                    TypeId var_type;
                    AstNode *value; // this is kept, but actually a OPERATOR_ASSIGN is inserted into the body
                    AstNode *scoped_expr;
                } local_scope;

                struct {
                    Str label;
                    AstNode *condition; // 0 indicates unconditional `loop`
                    AstNode *body;
                    AstNode *on_next_iter; // 0 indicates no action
                } loop;

                struct {
                    AstNode *condition;
                    AstNode *on_true;
                    AstNode *on_false;
                } if_else;

                struct {
                    AstNode *first_child;
                } list;

                Str ident;

                struct {
                    AstNode *struct_expr;
                    Str member;
                } member_access;

                struct {
                    AstNode *callee;
                    AstNode *first_arg;
                    bool is_indirect; /* Filled in by type checker */
                } call;

                struct {
                    AstNode *arg1;
                    AstNode *arg2;
                    enum BuiltinOp op;
                } builtin;

                struct {
                    AstNode *arg1;
                    AstNode *arg2;
                    enum OperatorOp op;
                } fe_operator;

                struct {
                    AstNode *arg;
                    TypeId to_type;
                } cast;

                struct {
                    Str asm_text;
                } asm_;
            };
        } expr;
    };

} AstNode;

typedef struct TokenCursor {
    Vec tokens;
    int next;
} TokenCursor;

extern void init_parser();
extern const char* operator_name(enum OperatorOp op);
/* Returns root AstNode */
extern AstNode *parse_file(const char *filename);
extern void print_ast(AstNode *node, int depth);
// includes self
extern int ast_node_sibling_size(AstNode *n);

#endif /* PARSER_H */
