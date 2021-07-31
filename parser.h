#ifndef __PARSER_H
#define __PARSER_H

#include "vec.h"
#include "str.h"

typedef int NodeIdx;

typedef struct FnArg {
    Str name;
    Str type;
} FnArg;

typedef enum ExprType {
    EXPR_LITERAL,
    EXPR_CALL
} ExprType;

typedef struct Call {
    Str fn_name;
    Vec/* NodeIdx */ args;
} Call;

typedef struct Expr {
    ExprType type;
    union {
        Str literal;
        Call call;
    };
} Expr;

typedef struct AstNode {
    enum AstType {
        AST_MODULE, AST_FN, AST_EXPR
    } type;

    union {
        struct {
            Vec nodes;
        } module;

        struct {
            Str name;
            Vec/*<FnArg>*/ args;
            Str ret;
            Vec/*<AstNode>*/ body;
        } fn;

        struct Expr expr;
    };

} AstNode;

typedef struct TokenCursor {
    Vec tokens;
    int next;
} TokenCursor;

extern void init_parser();
extern NodeIdx parse_module(TokenCursor *toks);
extern void print_ast(NodeIdx nidx, int depth);

#endif /* __PARSER_H */
