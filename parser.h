#ifndef __PARSER_H
#define __PARSER_H

#include "vec.h"
#include "str.h"
#include "types.h"

typedef int NodeIdx;
typedef struct Token Token;
typedef struct Program Program;

typedef struct AstNode {
    enum AstType {
        AST_MODULE, AST_FN, AST_FN_ARG, AST_EXPR, AST_DEF_VAR
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
            TypeId type;
        } fn;

        struct {
            Str name;
            TypeId type;
        } fn_arg;

        struct {
            Str name;
            TypeId type;
        } var_def;

        struct {
            enum ExprType {
                EXPR_LIST,  // like C comma operator, but using ;
                EXPR_IDENT,
                EXPR_LITERAL_VOID,
                EXPR_LITERAL_U8,
                EXPR_LITERAL_U16,
                EXPR_LITERAL_STR,
                EXPR_CALL,
                EXPR_ASM,
                EXPR_BUILTIN,
                EXPR_CAST,
                EXPR_IF_ELSE,
                EXPR_LOCAL_SCOPE,
                EXPR_WHILE_LOOP,
            } type;

            TypeId eval_type;

            union {
                struct {
                    Str var_name;
                    TypeId var_type;
                    NodeIdx scoped_expr;
                } local_scope;

                struct {
                    NodeIdx condition;
                    NodeIdx body;
                } while_loop;

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
                        BUILTIN_LOGICAL_AND,
                        BUILTIN_LOGICAL_OR,
                        BUILTIN_MUL,
                        BUILTIN_ASSIGN,
                        BUILTIN_EQ,
                        BUILTIN_NEQ,
                        BUILTIN_ARRAY_INDEXING,
                        BUILTIN_UNARY_NEG,
                    } op;
                } builtin;

                struct {
                    NodeIdx arg;
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
extern void parse_file(Program *prog, const char *filename);
extern void print_ast(NodeIdx nidx, int depth);
AstNode *get_node(NodeIdx idx);
// includes self
extern int ast_node_sibling_size(NodeIdx n);

#endif /* __PARSER_H */
