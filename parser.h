#ifndef __PARSER_H
#define __PARSER_H

#include "vec.h"
#include "str.h"

typedef int NodeIdx;

typedef struct AstNode {
    enum AstType {
        AST_MODULE, AST_FN, AST_FN_ARG, AST_EXPR
    } type;

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
            Str type;
        } fn_arg;

        struct {
            enum ExprType {
                EXPR_LIST,  // like C comma operator, but using ;
                EXPR_IDENT,
                EXPR_LITERAL,
                EXPR_CALL,
                EXPR_BUILTIN
            } type;

            union {
                struct {
                    NodeIdx first_child;
                } list;
                Str ident;
                Str literal;
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
                        BUILTIN_MUL
                    } op;
                } builtin;
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
