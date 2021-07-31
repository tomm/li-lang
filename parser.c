#include <assert.h>
#include "parser.h"
#include "tokenizer.h"

static Vec _node_alloc;

static Token tok_peek(TokenCursor *cursor, int ahead, bool skip_space) {
    int next = cursor->next;
    while (next < cursor->tokens.len) {
        Token t = *(Token*)vec_get(&cursor->tokens, next);
        next++;

        if (skip_space && t.type == T_SPACE) continue;
        if (ahead-- == 0) {
            return t;
        }
    }
    fprintf(stderr, "tok_peek() called at EOF position\n");
    fflush(stderr);
    return (Token) { T_EOF };
}

static Token tok_next(TokenCursor *cursor, bool skip_space) {
    while (cursor->next < cursor->tokens.len) {
        Token t = *(Token*)vec_get(&cursor->tokens, cursor->next);
        cursor->next++;

        if (skip_space && t.type == T_SPACE) continue;
        return t;
    }
    fprintf(stderr, "tok_next() called at EOF position\n");
    fflush(stderr);
    abort();
}

void init_parser() {
    _node_alloc = vec_init(sizeof(AstNode));
    // XXX never freed
}

static NodeIdx alloc_node() {
    AstNode n;
    memset(&n, 0, sizeof(AstNode));
    NodeIdx idx = _node_alloc.len;
    vec_push(&_node_alloc, &n);
    return idx;
}

static void parse_error(const char *msg, enum TokType expected, Token *got) {
    fprintf(stderr, "%d:%d: %s (expected %s but found %s)\n",
            got->line, got->col, msg, token_type_cstr(expected), token_type_cstr(got->type));
    exit(-1);
}

static AstNode *get_node(NodeIdx idx) { return vec_get(&_node_alloc, idx); }
static void set_node(NodeIdx idx, AstNode *n) { vec_set(&_node_alloc, idx, n); }

static Token chomp(TokenCursor *toks, enum TokType type) {
    Token t = tok_next(toks, true);
    if (t.type != type) {
        parse_error("Unexpected token", type, &t);
    }
    return t;
}

static bool check(TokenCursor *toks, enum TokType type) {
    Token t = tok_peek(toks, 0, true);
    return t.type == type;
}

static NodeIdx parse_literal_expression(TokenCursor *toks) {
    Token t = chomp(toks, T_DECIMAL);

    NodeIdx expr = alloc_node();
        
    set_node(expr, &(AstNode) {
        .type = AST_EXPR,
        .expr = {
            .type = EXPR_LITERAL,
            .literal = t.decimal
        }
    });
    return expr;
}

static NodeIdx parse_multiplicative_expression(TokenCursor *toks) {
    NodeIdx n = parse_literal_expression(toks);

    while (tok_peek(toks, 0, true).type == T_ASTERISK) {
        chomp(toks, T_ASTERISK);

        Vec/*<NodeIdx>*/ args = vec_init(sizeof(NodeIdx));

        vec_push(&args, &n);
        n = parse_literal_expression(toks);
        vec_push(&args, &n);
        
        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .expr = {
                .type = EXPR_CALL,
                .call = {
                    .fn_name = { .s = "*", .len = 1 },
                    .args = args,
                }
            }
        });
    }

    return n;
}

static NodeIdx parse_additive_expression(TokenCursor *toks) {
    NodeIdx n = parse_multiplicative_expression(toks);

    while (tok_peek(toks, 0, true).type == T_PLUS) {
        chomp(toks, T_PLUS);

        Vec/*<NodeIdx>*/ args = vec_init(sizeof(NodeIdx));

        vec_push(&args, &n);
        n = parse_multiplicative_expression(toks);
        vec_push(&args, &n);
        
        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .expr = {
                .type = EXPR_CALL,
                .call = {
                    .fn_name = { .s = "+", .len = 1 },
                    .args = args,
                }
            }
        });
    }

    return n;
}

static NodeIdx parse_expression(TokenCursor *toks) {
    return parse_additive_expression(toks);
}

static NodeIdx parse_function(TokenCursor *toks) {
    NodeIdx mod = alloc_node();

    // expect function name
    Token t = tok_next(toks, true);

    if (t.type != T_IDENT) {
        parse_error("Expected function name", T_IDENT, &t);
    }

    chomp(toks, T_LPAREN);

    Vec args = vec_init(sizeof(FnArg));
    {
        while (tok_peek(toks, 0, true).type == T_IDENT) {
            FnArg arg;
            arg.name = chomp(toks, T_IDENT).ident;
            chomp(toks, T_COLON);
            arg.type = chomp(toks, T_IDENT).ident;
            vec_push(&args, &arg);
            if (!check(toks, T_COMMA)) break;
            chomp(toks, T_COMMA);
        }
    }

    chomp(toks, T_RPAREN);

    Str ret; // return type
    if (check(toks, T_RARROW)) {
        chomp(toks, T_RARROW);

        Token ret_token = tok_next(toks, true);
        if (ret_token.type == T_IDENT) {
            ret = ret_token.ident;
        } else {
            parse_error("Expected return type", T_IDENT, &ret_token);
        }
    } else {
        ret = (Str) { .s = "void", .len = 4 };
    }

    chomp(toks, T_LBRACE);

    Vec body = vec_init(sizeof(NodeIdx));

    while (!check(toks, T_RBRACE)) {
        const NodeIdx n = parse_expression(toks);
        vec_push(&body, &n);
    }

    chomp(toks, T_RBRACE);

    set_node(mod, &(AstNode) {
        .type = AST_FN,
        .fn = {
            .name = t.ident,
            .args = args,
            .ret = ret,
            .body = body,
        }
    });

    return mod;
}

NodeIdx parse_module(TokenCursor *toks) {
    NodeIdx mod = alloc_node();
    Vec children = vec_init(sizeof(NodeIdx));

    for (;;) {
        Token t = tok_next(toks, true);
        switch (t.type) {
            case T_FN: {
                const NodeIdx child = parse_function(toks);
                vec_push(&children, &child);
                break;
            }
            case T_EOF:
                goto done;
            default:
                parse_error("Expected function or end of file", T_FN, &t);
        }
    }
done:

    set_node(mod, &(AstNode) {
        .type = AST_MODULE,
        .module = {
            .nodes = children
        }
    });

    return mod;
}

static void _indent(int depth) {
    for (int i=0; i<depth; ++i) { putchar(' '); }
}

static void print_fn_args(Vec *fn_args) {
    for (int i=0; i<fn_args->len; ++i) {
        FnArg a = *(FnArg*)vec_get(fn_args, i);
        Str_puts(a.name, stdout);
        fputs(":", stdout);
        Str_puts(a.type, stdout);
        if (i < fn_args->len-1) fputs(", ", stdout);
    }
}

void print_ast(NodeIdx nidx, int depth) {
    AstNode *node = get_node(nidx);

    switch (node->type) {
        case AST_MODULE:
            _indent(depth);
            printf("module\n");
            for (int i=0; i<node->module.nodes.len; ++i) {
                print_ast(*(NodeIdx*)vec_get(&node->module.nodes, i), depth+1);
            }
            break;
        case AST_FN:
            _indent(depth);
            printf("fn ");
            Str_puts(node->fn.name, stdout);
            printf("(");
            print_fn_args(&node->fn.args);
            printf(") -> ");
            Str_puts(node->fn.ret, stdout);
            printf("\n");
            for (int i=0; i<node->fn.body.len; ++i) {
                print_ast(*(NodeIdx*)vec_get(&node->fn.body, i), depth+1);
            }
            break;
        case AST_EXPR:
            _indent(depth+1);
            switch (node->expr.type) {
                case EXPR_LITERAL:
                    printf("expr_literal ");
                    Str_puts(node->expr.literal, stdout);
                    printf("\n");
                    break;
                case EXPR_CALL:
                    printf("expr_call ");
                    Str_puts(node->expr.call.fn_name, stdout);
                    printf("()\n");
                    for (int i=0; i<node->expr.call.args.len; ++i) {
                        print_ast(*(NodeIdx*)vec_get(&node->expr.call.args, i), depth+1);
                    }
                    break;
            }
            break;
    }
}
