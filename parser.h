#ifndef __PARSER_H
#define __PARSER_H

#include "vec.h"
#include "str.h"

typedef int NodeIdx;
typedef struct Token Token;

typedef struct AstNode {
    enum AstType {
        AST_MODULE, AST_FN, AST_FN_ARG, AST_EXPR, AST_DEF_VAR, AST_TYPENAME
    } type;

    const Token *start_token; /* for error reporting */
    NodeIdx next_sibling; /* 0 is end */

    union {
        struct {
            NodeIdx first_child;
        } module;

        struct {
            Str name;
            NodeIdx first_arg;
            NodeIdx body;
            Str ret;
        } fn;

        struct {
            Str name;
            NodeIdx typename_;
        } fn_arg;

        struct {
            Str name;
            NodeIdx typename_;
        } var_def;

        struct {
            Str name;
        } typename_;

        struct {
            enum ExprType {
                EXPR_LIST,  // like C comma operator, but using ;
                EXPR_IDENT,
                EXPR_LITERAL_VOID,
                EXPR_LITERAL_U8,
                EXPR_LITERAL_U16,
                EXPR_LITERAL_STR,
                EXPR_CALL,
                EXPR_BUILTIN,
                EXPR_CAST,
                EXPR_IF_ELSE,
            } type;

            union {
                struct {
                    NodeIdx condition;
                    NodeIdx on_true;
                    NodeIdx on_false;
                } if_else;
                struct {
                    NodeIdx first_child;
                } list;
                Str ident;
                int literal_int;
                Str literal_str;
                struct {
                    NodeIdx callee;
                    NodeIdx first_arg;
                } call;
                struct {
                    NodeIdx first_arg;
                    enum BuiltinOp {
                        BUILTIN_ADD,
                        BUILTIN_SUB,
                        BUILTIN_BITAND,
                        BUILTIN_BITOR,
                        BUILTIN_BITXOR,
                        BUILTIN_MUL,
                        BUILTIN_ASSIGN,
                        BUILTIN_EQ,
                        BUILTIN_NEQ,
                    } op;
                } builtin;
                struct {
                    NodeIdx arg;
                    Str to_type;
                } cast;
            };
        } expr;
    };

} AstNode;

typedef struct TokenCursor {
    Vec tokens;
    int next;
} TokenCursor;

extern void init_parser();
extern NodeIdx parse_module(TokenCursor *toks);
extern void print_ast(NodeIdx nidx, int depth);
AstNode *get_node(NodeIdx idx);

#endif /* __PARSER_H */
