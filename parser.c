#include <assert.h>
#include "parser.h"
#include "tokenizer.h"

static Vec _node_alloc;

static const Token *tok_peek(TokenCursor *cursor, int ahead, bool skip_space) {
    int next = cursor->next;
    while (next < cursor->tokens.len) {
        const Token *t = (Token*)vec_get(&cursor->tokens, next);
        next++;

        if (skip_space && t->type == T_SPACE) continue;
        if (ahead-- == 0) {
            return t;
        }
    }
    fprintf(stderr, "tok_peek() called at EOF position\n");
    fflush(stderr);
    abort();
}

static const Token *tok_next(TokenCursor *cursor, bool skip_space) {
    while (cursor->next < cursor->tokens.len) {
        const Token *t = (Token*)vec_get(&cursor->tokens, cursor->next);
        cursor->next++;

        if (skip_space && t->type == T_SPACE) continue;
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

static void parse_error(const char *msg, enum TokType expected, const Token *got) {
    fprintf(stderr, "%d:%d: %s (expected %s but found %s)\n",
            got->line, got->col, msg, token_type_cstr(expected), token_type_cstr(got->type));
    exit(-1);
}

AstNode *get_node(NodeIdx idx) { return vec_get(&_node_alloc, idx); }
static void set_node(NodeIdx idx, AstNode *n) { vec_set(&_node_alloc, idx, n); }

/* child node linked list logic */
typedef struct ChildCursor {
    NodeIdx first_child;
    NodeIdx last_child;
} ChildCursor;
static ChildCursor ChildCursor_init() { return (ChildCursor) { 0, 0 }; }
static void ChildCursor_append(ChildCursor *cursor, NodeIdx child) {
    if (child != 0) {
        if (cursor->first_child == 0) {
            cursor->first_child = child;
        } else {
            get_node(cursor->last_child)->next_sibling = child;
        }
        cursor->last_child = child;
    }
}

static const Token *chomp(TokenCursor *toks, enum TokType type) {
    const Token *t = tok_next(toks, true);
    if (t->type != type) {
        parse_error("Unexpected token", type, t);
    }
    return t;
}

static bool check(TokenCursor *toks, enum TokType type) {
    return tok_peek(toks, 0, true)->type == type;
}

static NodeIdx parse_primary_expression(TokenCursor *toks) {
    const Token *t = tok_next(toks, true);
    NodeIdx expr = alloc_node();

    switch (t->type) {
        case T_DECIMAL:
            set_node(expr, &(AstNode) {
                .start_token = t,
                .type = AST_EXPR,
                .expr = {
                    .type = EXPR_LITERAL,
                    .literal = t->decimal
                }
            });
            break;
        case T_IDENT:
            set_node(expr, &(AstNode) {
                .start_token = t,
                .type = AST_EXPR,
                .expr = {
                    .type = EXPR_IDENT,
                    .ident = t->ident
                }
            });
            break;
        default:
            parse_error("Expected primary expression", 0, t);
    }
        
    return expr;
}

static NodeIdx parse_expression(TokenCursor *toks);

static NodeIdx parse_postfix_expression(TokenCursor *toks) {
    NodeIdx n = parse_primary_expression(toks);

    // function call
    if (tok_peek(toks, 0, true)->type == T_LPAREN) {
        const Token *start_token = chomp(toks, T_LPAREN);

        ChildCursor args = ChildCursor_init();
        while (tok_peek(toks, 0, true)->type != T_RPAREN) {
            ChildCursor_append(&args, parse_expression(toks));
            if (tok_peek(toks, 0, true)->type != T_COMMA) {
                break;
            }
            chomp(toks, T_COMMA);
        }

        chomp(toks, T_RPAREN);

        NodeIdx call = alloc_node();
        set_node(call, &(AstNode) {
            .start_token = start_token,
            .type = AST_EXPR,
            .expr = {
                .type = EXPR_CALL,
                .call = {
                    .callee = n,
                    .first_arg = args.first_child,
                }
            }
        });
        return call;
    }
    else {
        return n;
    }
}

static NodeIdx parse_multiplicative_expression(TokenCursor *toks) {
    NodeIdx n = parse_postfix_expression(toks);

    while (tok_peek(toks, 0, true)->type == T_ASTERISK) {
        const Token *start_token = chomp(toks, T_ASTERISK);

        ChildCursor args = ChildCursor_init();
        ChildCursor_append(&args, n);
        ChildCursor_append(&args, parse_postfix_expression(toks));

        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = start_token,
            .expr = {
                .type = EXPR_BUILTIN,
                .builtin = {
                    .op = BUILTIN_MUL,
                    .first_arg = args.first_child,
                }
            }
        });
    }

    return n;
}

static NodeIdx parse_additive_expression(TokenCursor *toks) {
    NodeIdx n = parse_multiplicative_expression(toks);

    while (tok_peek(toks, 0, true)->type == T_PLUS ||
           tok_peek(toks, 0, true)->type == T_MINUS ||
           tok_peek(toks, 0, true)->type == T_BITAND ||
           tok_peek(toks, 0, true)->type == T_BITOR ||
           tok_peek(toks, 0, true)->type == T_BITXOR
    ) {
        const Token *t = tok_next(toks, true);

        ChildCursor args = ChildCursor_init();
        ChildCursor_append(&args, n);
        ChildCursor_append(&args, parse_multiplicative_expression(toks));
        
        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_BUILTIN,
                .builtin = {
                    .op = (
                        t->type == T_PLUS ? BUILTIN_ADD
                        : t->type == T_MINUS ? BUILTIN_SUB
                        : t->type == T_BITAND ? BUILTIN_BITAND
                        : t->type == T_BITOR ? BUILTIN_BITOR
                        : t->type == T_BITXOR ? BUILTIN_BITXOR
                        : (abort(), BUILTIN_ADD)
                    ),
                    .first_arg = args.first_child
                }
            }
        });
    }

    return n;
}

static NodeIdx parse_expression(TokenCursor *toks) {
    return parse_additive_expression(toks);
}

static NodeIdx parse_list_expression(TokenCursor *toks) {
    ChildCursor exprs = ChildCursor_init();
    
    const Token *start_token = tok_peek(toks, 0, true);

    while (tok_peek(toks, 0, true)->type != T_RBRACE) {
        ChildCursor_append(&exprs, parse_expression(toks));
        if (tok_peek(toks, 0, true)->type == T_SEMICOLON) {
            chomp(toks, T_SEMICOLON);
        } else {
            break;
        }
    }

    NodeIdx list = alloc_node();
    set_node(list, &(AstNode) {
        .type = AST_EXPR,
        .start_token = start_token,
        .expr = {
            .type = EXPR_LIST,
            .list = {
                .first_child = exprs.first_child
            }
        }
    });
    return list;
}

static NodeIdx parse_function(TokenCursor *toks) {
    NodeIdx mod = alloc_node();

    // expect function name
    const Token *t = tok_next(toks, true);

    if (t->type != T_IDENT) {
        parse_error("Expected function name", T_IDENT, t);
    }

    chomp(toks, T_LPAREN);

    ChildCursor args = ChildCursor_init();
    {
        while (tok_peek(toks, 0, true)->type == T_IDENT) {
            NodeIdx a = alloc_node();
            AstNode *n = get_node(a);
            n->type = AST_FN_ARG;
            n->fn_arg.name = chomp(toks, T_IDENT)->ident;
            chomp(toks, T_COLON);
            n->fn_arg.type = chomp(toks, T_IDENT)->ident;

            ChildCursor_append(&args, a);

            if (!check(toks, T_COMMA)) break;
            chomp(toks, T_COMMA);
        }
    }

    chomp(toks, T_RPAREN);

    Str ret; // return type
    if (check(toks, T_RARROW)) {
        chomp(toks, T_RARROW);

        const Token *ret_token = tok_next(toks, true);
        if (ret_token->type == T_IDENT) {
            ret = ret_token->ident;
        } else {
            parse_error("Expected return type", T_IDENT, ret_token);
        }
    } else {
        ret = (Str) { .s = "void", .len = 4 };
    }

    chomp(toks, T_LBRACE);
    NodeIdx body = parse_list_expression(toks);
    chomp(toks, T_RBRACE);

    set_node(mod, &(AstNode) {
        .type = AST_FN,
        .fn = {
            .name = t->ident,
            .first_arg = args.first_child,
            .body = body,
            .ret = ret,
        }
    });

    return mod;
}

NodeIdx parse_module(TokenCursor *toks) {
    NodeIdx mod = alloc_node();
    ChildCursor children = ChildCursor_init();

    for (;;) {
        const Token *t = tok_next(toks, true);

        switch (t->type) {
            case T_FN:
                ChildCursor_append(&children, parse_function(toks));
                break;
            case T_EOF:
                goto done;
            default:
                parse_error("Expected function or end of file", T_FN, t);
        }
    }

done:

    set_node(mod, &(AstNode) {
        .type = AST_MODULE,
        .module = {
            .first_child = children.first_child
        }
    });

    return mod;
}

static void _indent(int depth) {
    for (int i=0; i<depth; ++i) { putchar(' '); }
}

void print_ast(NodeIdx nidx, int depth) {
    AstNode *node = get_node(nidx);

    switch (node->type) {
        case AST_MODULE:
            _indent(depth);
            printf("module\n");
            for (NodeIdx child=node->module.first_child; child != 0; child = get_node(child)->next_sibling) {
                print_ast(child, depth+1);
            }
            break;
        case AST_FN_ARG:
            Str_puts(node->fn_arg.name, stdout);
            fputs(":", stdout);
            Str_puts(node->fn_arg.type, stdout);
            break;
        case AST_FN:
            _indent(depth);
            printf("fn ");
            Str_puts(node->fn.name, stdout);
            printf("(");
            for (NodeIdx child=node->fn.first_arg; child != 0; child = get_node(child)->next_sibling) {
                print_ast(child, depth+1);
                fputs(", ", stdout);
            }
            printf(") -> ");
            Str_puts(node->fn.ret, stdout);
            printf("\n");
            print_ast(node->fn.body, depth+1);
            break;
        case AST_EXPR:
            _indent(depth+1);
            switch (node->expr.type) {
                case EXPR_LIST:
                    printf("expr_list\n");
                    for (NodeIdx child=node->expr.list.first_child; child != 0; child = get_node(child)->next_sibling) {
                        print_ast(child, depth+1);
                    }
                    break;
                case EXPR_IDENT:
                    printf("expr_ident ");
                    Str_puts(node->expr.ident, stdout);
                    printf("\n");
                    break;
                case EXPR_LITERAL:
                    printf("expr_literal ");
                    Str_puts(node->expr.literal, stdout);
                    printf("\n");
                    break;
                case EXPR_CALL:
                    printf("expr_call\n");
                    print_ast(node->expr.call.callee, depth+1);
                    _indent(depth+2);
                    printf("args\n");
                    for (NodeIdx arg=node->expr.call.first_arg; arg != 0; arg = get_node(arg)->next_sibling) {
                        print_ast(arg, depth+2);
                    }
                    break;
                case EXPR_BUILTIN:
                    printf("expr_builtin (op=%d)\n", node->expr.builtin.op);
                    _indent(depth+2);
                    printf("args\n");
                    for (NodeIdx arg=node->expr.builtin.first_arg; arg != 0; arg = get_node(arg)->next_sibling) {
                        print_ast(arg, depth+2);
                    }
                    break;
            }
            break;
    }
}
