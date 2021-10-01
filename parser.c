#include <assert.h>
#include "parser.h"
#include "tokenizer.h"
#include "error.h"
#include "program.h"

static Vec _node_alloc;

const char* builtin_name(enum BuiltinOp op) {
    switch (op) {
        case BUILTIN_ADD: return "add";
        case BUILTIN_SUB: return "subtract";
        case BUILTIN_BITAND: return "bitwise and";
        case BUILTIN_BITOR: return "bitwise or";
        case BUILTIN_BITXOR: return "bitwise xor";
        case BUILTIN_LOGICAL_AND: return "logical and";
        case BUILTIN_LOGICAL_OR: return "logical or";
        case BUILTIN_UNARY_LOGICAL_NOT: return "logical not";
        case BUILTIN_MUL: return "multiply";
        case BUILTIN_DIV: return "divide";
        case BUILTIN_MODULO: return "modulo";
        case BUILTIN_ASSIGN: return "assignment";
        case BUILTIN_PLUSASSIGN: return "+=";
        case BUILTIN_MINUSASSIGN: return "-=";
        case BUILTIN_MULASSIGN: return "*=";
        case BUILTIN_DIVASSIGN: return "/=";
        case BUILTIN_MODASSIGN: return "%=";
        case BUILTIN_LSHIFTASSIGN: return "<<=";
        case BUILTIN_RSHIFTASSIGN: return ">>=";
        case BUILTIN_BITANDASSIGN: return "&=";
        case BUILTIN_BITORASSIGN: return "|=";
        case BUILTIN_BITXORASSIGN: return "^=";
        case BUILTIN_EQ: return "equality";
        case BUILTIN_NEQ: return "inequality";
        case BUILTIN_LT: return "less than";
        case BUILTIN_GT: return "greater than";
        case BUILTIN_LTE: return "less than or equal to";
        case BUILTIN_GTE: return "greater than or equal to";
        case BUILTIN_SHIFT_LEFT: return "left shift";
        case BUILTIN_SHIFT_RIGHT: return "right shift";
        case BUILTIN_ARRAY_INDEXING: return "array indexing";
        case BUILTIN_UNARY_NEG: return "unary negation";
        case BUILTIN_UNARY_ADDRESSOF: return "address of";
        case BUILTIN_UNARY_DEREF: return "pointer dereference";
        case BUILTIN_UNARY_BITNOT: return "bitwise not";
    }
    assert(false);
}

static Str read_file(FILE *f) {
    fseek(f, 0, SEEK_END);
    const size_t len = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *buf = malloc(len);
    const size_t num_read = fread(buf, 1, len, f);
    assert(num_read == len);

    return (Str) { buf, len };
}

static const Token *tok_peek_whitespace_sensitive(TokenCursor *cursor, int ahead) {
    int next = cursor->next;
    while (next < cursor->tokens.len) {
        const Token *t = (Token*)vec_get(&cursor->tokens, next);
        next++;

        if (ahead-- == 0) {
            return t;
        }
    }
    fprintf(stderr, "tok_peek_whitespace_sensitive() called at EOF position\n");
    fflush(stderr);
    assert(false);
}

static void ignore_whitespace(TokenCursor *cursor) {
    while (cursor->next < cursor->tokens.len) {
        const Token *t = (Token*)vec_get(&cursor->tokens, cursor->next);
        if (t->type != T_SPACE) break;
        cursor->next++;
    }
}

static const Token *tok_peek(TokenCursor *cursor, int ahead) {
    int next = cursor->next;
    while (next < cursor->tokens.len) {
        const Token *t = (Token*)vec_get(&cursor->tokens, next);
        next++;
        if (t->type == T_SPACE) continue;

        if (ahead-- == 0) {
            return t;
        }
    }
    fprintf(stderr, "tok_peek() called at EOF position\n");
    fflush(stderr);
    assert(false);
}

static const Token *tok_next(TokenCursor *cursor) {
    while (cursor->next < cursor->tokens.len) {
        const Token *t = (Token*)vec_get(&cursor->tokens, cursor->next);
        cursor->next++;
        if (t->type == T_SPACE) continue;

        return t;
    }
    fprintf(stderr, "tok_next() called at EOF position\n");
    fflush(stderr);
    assert(false);
}

void init_parser() {
    _node_alloc = vec_init(sizeof(AstNode));
    // XXX never freed

    init_types();
}

static NodeIdx alloc_node() {
    AstNode n;
    memset(&n, 0, sizeof(AstNode));
    NodeIdx idx = _node_alloc.len;
    vec_push(&_node_alloc, &n);
    return idx;
}

static void parse_error(const char *msg, enum TokType expected, const Token *got) __attribute__((noreturn));
static void parse_error(const char *msg, enum TokType expected, const Token *got)
{
    fatal_error(got, "%s: expected %s but found %s", msg, token_type_cstr(expected), token_type_cstr(got->type));
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
    const Token *t = tok_next(toks);
    if (t->type != type) {
        parse_error("Unexpected token", type, t);
    }
    return t;
}

static bool check(TokenCursor *toks, enum TokType type) {
    return tok_peek(toks, 0)->type == type;
}

static TypeId parse_type(TokenCursor *toks);
static NodeIdx parse_expression(TokenCursor *toks);
static NodeIdx parse_list_expression(TokenCursor *toks, enum TokType terminator);

static NodeIdx parse_asm_expression(TokenCursor *toks) {
    // 'asm' identifier already chomped
    chomp(toks, T_LPAREN);
    const Token *asm_text = chomp(toks, T_LITERAL_STR);
    chomp(toks, T_RPAREN);

    NodeIdx expr = alloc_node();
    set_node(expr, &(AstNode) {
        .start_token = asm_text,
        .type = AST_EXPR,
        .expr = {
            .type = EXPR_ASM,
            .asm_ = {
                .asm_text = asm_text->str_literal
            }
        }
    });
    return expr;
}

static NodeIdx parse_primary_expression(TokenCursor *toks) {
    const Token *t = tok_next(toks);

    switch (t->type) {
        case T_LITERAL_U8:
            {
                NodeIdx expr = alloc_node();
                set_node(expr, &(AstNode) {
                    .start_token = t,
                    .type = AST_EXPR,
                    .expr = {
                        .type = EXPR_LITERAL,
                        .literal = {
                            .type = LIT_U8,
                            .literal_int = t->int_literal
                        }
                    }
                });
                return expr;
            }
        case T_LITERAL_U16:
            {
                NodeIdx expr = alloc_node();
                set_node(expr, &(AstNode) {
                    .start_token = t,
                    .type = AST_EXPR,
                    .expr = {
                        .type = EXPR_LITERAL,
                        .literal = {
                            .type = LIT_U16,
                            .literal_int = t->int_literal
                        }
                    }
                });
                return expr;
            }
        case T_LITERAL_ANY_INT:
            {
                NodeIdx expr = alloc_node();
                set_node(expr, &(AstNode) {
                    .start_token = t,
                    .type = AST_EXPR,
                    .expr = {
                        .type = EXPR_LITERAL,
                        .literal = {
                            .type = LIT_INT_ANY,
                            .literal_int = t->int_literal
                        }
                    }
                });
                return expr;
            }
        case T_LSQBRACKET:
            {
                ChildCursor args = ChildCursor_init();
                while (tok_peek(toks, 0)->type != T_RSQBRACKET) {
                    ChildCursor_append(&args, parse_expression(toks));
                    if (tok_peek(toks, 0)->type == T_COMMA) {
                        chomp(toks, T_COMMA);
                    } else {
                        break;
                    }
                }
                chomp(toks, T_RSQBRACKET);

                NodeIdx expr = alloc_node();
                set_node(expr, &(AstNode) {
                    .start_token = t,
                    .type = AST_EXPR,
                    .expr = {
                        .type = EXPR_LITERAL,
                        .literal = {
                            .type = LIT_ARRAY,
                            .literal_array_first_val = args.first_child
                        }
                    }
                });
                return expr;
            }
        case T_LITERAL_STR:
            {
                NodeIdx expr = alloc_node();
                set_node(expr, &(AstNode) {
                    .start_token = t,
                    .type = AST_EXPR,
                    .expr = {
                        .type = EXPR_LITERAL,
                        .literal = {
                            .type = LIT_STR,
                            .literal_str = t->str_literal
                        }
                    }
                });
                return expr;
            }
            break;
        case T_IDENT:
            {
                if (Str_eq(t->ident, "asm")) {
                    return parse_asm_expression(toks);
                } else {
                    NodeIdx expr = alloc_node();
                    set_node(expr, &(AstNode) {
                        .start_token = t,
                        .type = AST_EXPR,
                        .expr = {
                            .type = EXPR_IDENT,
                            .ident = t->ident
                        }
                    });
                    return expr;
                }
            }
        case T_LPAREN:
            {
                NodeIdx expr = parse_list_expression(toks, T_RPAREN);
                chomp(toks, T_RPAREN);
                return expr;
            }
        case T_LBRACE:
            {
                NodeIdx expr = parse_list_expression(toks, T_RBRACE);
                chomp(toks, T_RBRACE);
                return expr;
            }
        case T_BREAK:
        case T_CONTINUE:
            {
                Str label = tok_peek(toks, 0)->type == T_JUMP_LABEL
                    ? tok_next(toks)->label : (Str) { .s = NULL, .len = 0 };

                NodeIdx expr = alloc_node();
                set_node(expr, &(AstNode) {
                    .start_token = t,
                    .type = AST_EXPR,
                    .expr = {
                        .type = EXPR_GOTO,
                        .goto_ = {
                            .is_continue = t->type == T_CONTINUE,
                            .label = label,
                            .target = 0, // resolved later
                        }
                    }
                });
                return expr;
            }
            break;
        case T_RETURN:
            {
                NodeIdx val = parse_expression(toks);

                NodeIdx expr = alloc_node();
                set_node(expr, &(AstNode) {
                    .start_token = t,
                    .type = AST_EXPR,
                    .expr = {
                        .type = EXPR_RETURN,
                        .return_ = {
                            .val = val
                        }
                    }
                });
                return expr;
            }
            break;
        default:
            parse_error("Expected primary expression", 0, t);
    }
}

static NodeIdx parse_postfix_expression(TokenCursor *toks) {
    NodeIdx n = parse_primary_expression(toks);

    for (;;) {
        const Token *start_token = tok_peek(toks, 0);

        // function calling
        if (start_token->type == T_LPAREN) {
            chomp(toks, T_LPAREN);

            ChildCursor args = ChildCursor_init();
            while (tok_peek(toks, 0)->type != T_RPAREN) {
                ChildCursor_append(&args, parse_expression(toks));
                if (tok_peek(toks, 0)->type != T_COMMA) {
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

            n = call;
        }

        // array indexing
        else if (start_token->type == T_LSQBRACKET) {
            chomp(toks, T_LSQBRACKET);
            ChildCursor args = ChildCursor_init();
            ChildCursor_append(&args, n);
            ChildCursor_append(&args, parse_expression(toks));
            chomp(toks, T_RSQBRACKET);

            n = alloc_node();
            set_node(n, &(AstNode) {
                .type = AST_EXPR,
                .start_token = start_token,
                .expr = {
                    .type = EXPR_BUILTIN,
                    .builtin = {
                        .op = BUILTIN_ARRAY_INDEXING,
                        .first_arg = args.first_child,
                    }
                }
            });
        }

        // array to pointer cast (&x[0] == x$)
            /*
        else if (start_token->type == T_DOLLAR) {
            NodeIdx child = n;
            n = alloc_node();
            set_node(n, &(AstNode) {
                .type = AST_EXPR,
                .start_token = start_token,
                .expr = {
                    .type = EXPR_CAST,
                    .cast = {
                        .arg = child,
                        .to_type = 0, // filled in during type check
                    }
                }
            });
        }
            */
        else {
            return n;
        }
    }
}

static NodeIdx parse_unary_expression(TokenCursor *toks) {
    const Token *t = tok_peek(toks, 0);

    if ((t->type == T_MINUS) ||
        (t->type == T_ASTERISK) ||
        (t->type == T_TILDE) ||
        (t->type == T_EXCLAMATION) ||
        (t->type == T_AMPERSAND)) {
        tok_next(toks);
        ChildCursor args = ChildCursor_init();
        ChildCursor_append(&args, parse_unary_expression(toks));

        NodeIdx n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_BUILTIN,
                .builtin = {
                    .op = t->type == T_MINUS
                        ? BUILTIN_UNARY_NEG
                        : t->type == T_EXCLAMATION
                        ? BUILTIN_UNARY_LOGICAL_NOT
                        : t->type == T_TILDE
                        ? BUILTIN_UNARY_BITNOT
                        : t->type == T_ASTERISK
                        ? BUILTIN_UNARY_DEREF
                        : t->type == T_AMPERSAND
                        ? BUILTIN_UNARY_ADDRESSOF
                        : (assert(false), 0),
                    .first_arg = args.first_child,
                }
            }
        });
        return n;
    } else {
        return parse_postfix_expression(toks);
    }
}

static NodeIdx parse_cast_expression(TokenCursor *toks) {
    NodeIdx n = parse_unary_expression(toks);

    while (tok_peek(toks, 0)->type == T_AS) {
        const Token *start_token = chomp(toks, T_AS);
        TypeId to_type = parse_type(toks);

        NodeIdx child = n;
        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = start_token,
            .expr = {
                .type = EXPR_CAST,
                .cast = {
                    .arg = child,
                    .to_type = to_type
                }
            }
        });
    }

    return n;
}

static NodeIdx parse_multiplicative_expression(TokenCursor *toks) {
    NodeIdx n = parse_cast_expression(toks);

    while (tok_peek(toks, 0)->type == T_ASTERISK ||
           tok_peek(toks, 0)->type == T_SLASH ||
           tok_peek(toks, 0)->type == T_PERCENT) {
        const Token *t = tok_next(toks);

        ChildCursor args = ChildCursor_init();
        ChildCursor_append(&args, n);
        ChildCursor_append(&args, parse_cast_expression(toks));

        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_BUILTIN,
                .builtin = {
                    .op = t->type == T_ASTERISK
                        ? BUILTIN_MUL
                        : t->type == T_SLASH
                        ? BUILTIN_DIV
                        : t->type == T_PERCENT
                        ? BUILTIN_MODULO
                        : (assert(false), 0),
                    .first_arg = args.first_child,
                }
            }
        });
    }

    return n;
}

static NodeIdx parse_additive_expression(TokenCursor *toks) {
    NodeIdx n = parse_multiplicative_expression(toks);


    for (;;) {
        // needed so tok_peek_whitespace_sensitive isn't confused by whitespace BEFORE
        // the first token
        ignore_whitespace(toks);
        if (!(
            tok_peek(toks, 0)->type == T_PLUS ||
            tok_peek(toks, 0)->type == T_MINUS ||
            tok_peek(toks, 0)->type == T_SHIFT_LEFT ||
            tok_peek(toks, 0)->type == T_SHIFT_RIGHT ||
            (
                tok_peek(toks, 0)->type == T_AMPERSAND &&
                tok_peek_whitespace_sensitive(toks, 1)->type != T_AMPERSAND
            ) || (
                tok_peek(toks, 0)->type == T_PIPE &&
                tok_peek_whitespace_sensitive(toks, 1)->type != T_PIPE
            ) || (
                tok_peek(toks, 0)->type == T_ACUTE &&
                tok_peek_whitespace_sensitive(toks, 1)->type != T_ACUTE
            )
        )) {
            break;
        }
        const Token *t = tok_next(toks);

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
                        : t->type == T_SHIFT_LEFT ? BUILTIN_SHIFT_LEFT
                        : t->type == T_SHIFT_RIGHT ? BUILTIN_SHIFT_RIGHT
                        : t->type == T_AMPERSAND ? BUILTIN_BITAND
                        : t->type == T_PIPE ? BUILTIN_BITOR
                        : t->type == T_ACUTE ? BUILTIN_BITXOR
                        : (assert(false), 0)
                    ),
                    .first_arg = args.first_child
                }
            }
        });
    }

    return n;
}

static NodeIdx parse_logical_expression(TokenCursor *toks) {
    NodeIdx n = parse_additive_expression(toks);

    for (;;) {
        ignore_whitespace(toks);

        if (!(
            (
                tok_peek(toks, 0)->type == T_PIPE &&
                tok_peek_whitespace_sensitive(toks, 1)->type == T_PIPE
            ) || (
                tok_peek(toks, 0)->type == T_AMPERSAND &&
                tok_peek_whitespace_sensitive(toks, 1)->type == T_AMPERSAND
            )
        )) {
            break;
        }
        const Token *t = tok_next(toks);
        tok_next(toks); // eat second character of && or ||

        ChildCursor args = ChildCursor_init();
        ChildCursor_append(&args, n);
        ChildCursor_append(&args, parse_additive_expression(toks));
        
        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_BUILTIN,
                .builtin = {
                    .op = t->type == T_PIPE ? BUILTIN_LOGICAL_OR : BUILTIN_LOGICAL_AND,
                    .first_arg = args.first_child
                }
            }
        });
    }

    return n;
}

static NodeIdx parse_comparison_expression(TokenCursor *toks) {
    NodeIdx n = parse_logical_expression(toks);

    while (tok_peek(toks, 0)->type == T_EQ ||
           tok_peek(toks, 0)->type == T_NEQ ||
           tok_peek(toks, 0)->type == T_GTE ||
           tok_peek(toks, 0)->type == T_LTE ||
           tok_peek(toks, 0)->type == T_GT ||
           tok_peek(toks, 0)->type == T_LT
    ) {
        const Token *t = tok_next(toks);

        ChildCursor args = ChildCursor_init();
        ChildCursor_append(&args, n);
        ChildCursor_append(&args, parse_logical_expression(toks));
        
        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_BUILTIN,
                .builtin = {
                    .op = t->type == T_EQ
                          ? BUILTIN_EQ
                          : t->type == T_NEQ
                          ? BUILTIN_NEQ
                          : t->type == T_GT
                          ? BUILTIN_GT
                          : t->type == T_LT
                          ? BUILTIN_LT
                          : t->type == T_GTE
                          ? BUILTIN_GTE
                          : t->type == T_LTE
                          ? BUILTIN_LTE
                          : (assert(false), 0),
                    .first_arg = args.first_child
                }
            }
        });
    }

    return n;
}

static NodeIdx parse_conditional_expression(TokenCursor *toks) {
    Str label = { .s = 0 };

    if (tok_peek(toks, 0)->type == T_JUMP_LABEL &&
        tok_peek(toks, 1)->type == T_COLON &&
        tok_peek(toks, 2)->type == T_WHILE) {

        label = chomp(toks, T_JUMP_LABEL)->label;
        chomp(toks, T_COLON);
    }

    if (tok_peek(toks, 0)->type == T_WHILE) {
        const Token *t = chomp(toks, T_WHILE);
        NodeIdx condition = parse_expression(toks);
        chomp(toks, T_LBRACE);
        NodeIdx body = parse_list_expression(toks, T_RBRACE);
        chomp(toks, T_RBRACE);

        NodeIdx while_loop = alloc_node();
        set_node(while_loop, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_WHILE_LOOP,
                .while_loop = {
                    .label = label,
                    .condition = condition,
                    .body = body,
                }
            }
        });

        return while_loop;
    } else if (tok_peek(toks, 0)->type == T_IF) {
        const Token *t = chomp(toks, T_IF);

        NodeIdx condition = parse_expression(toks);
        chomp(toks, T_LBRACE);
        NodeIdx on_true = parse_list_expression(toks, T_RBRACE);
        chomp(toks, T_RBRACE);
        NodeIdx on_false = 0;

        if (tok_peek(toks, 0)->type == T_ELSE) {
            chomp(toks, T_ELSE);
            chomp(toks, T_LBRACE);
            on_false = parse_list_expression(toks, T_RBRACE);
            chomp(toks, T_RBRACE);
        }

        NodeIdx if_else = alloc_node();
        set_node(if_else, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_IF_ELSE,
                .if_else = {
                    .condition = condition,
                    .on_true = on_true,
                    .on_false = on_false
                }
            }
        });

        return if_else;
    } else {
        return parse_comparison_expression(toks);
    }
}

static NodeIdx parse_assignment_expression(TokenCursor *toks) {
    NodeIdx n = parse_conditional_expression(toks);

    // right associative
    if (tok_peek(toks, 0)->type == T_ASSIGN ||
        tok_peek(toks, 0)->type == T_MULASSIGN ||
        tok_peek(toks, 0)->type == T_DIVASSIGN ||
        tok_peek(toks, 0)->type == T_MODASSIGN ||
        tok_peek(toks, 0)->type == T_LSHIFTASSIGN ||
        tok_peek(toks, 0)->type == T_RSHIFTASSIGN ||
        tok_peek(toks, 0)->type == T_BITANDASSIGN ||
        tok_peek(toks, 0)->type == T_BITORASSIGN ||
        tok_peek(toks, 0)->type == T_BITXORASSIGN ||
        tok_peek(toks, 0)->type == T_PLUSASSIGN ||
        tok_peek(toks, 0)->type == T_MINUSASSIGN)
    {
        const Token *t = tok_next(toks);

        ChildCursor args = ChildCursor_init();
        ChildCursor_append(&args, n);
        ChildCursor_append(&args, parse_assignment_expression(toks));
        
        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_BUILTIN,
                .builtin = {
                    .op = t->type == T_ASSIGN
                        ? BUILTIN_ASSIGN
                        : t->type == T_PLUSASSIGN
                        ? BUILTIN_PLUSASSIGN
                        : t->type == T_MINUSASSIGN
                        ? BUILTIN_MINUSASSIGN
                        : t->type == T_MULASSIGN
                        ? BUILTIN_MULASSIGN
                        : t->type == T_DIVASSIGN
                        ? BUILTIN_DIVASSIGN
                        : t->type == T_MODASSIGN
                        ? BUILTIN_MODASSIGN
                        : t->type == T_LSHIFTASSIGN
                        ? BUILTIN_LSHIFTASSIGN
                        : t->type == T_RSHIFTASSIGN
                        ? BUILTIN_RSHIFTASSIGN
                        : t->type == T_BITANDASSIGN
                        ? BUILTIN_BITANDASSIGN
                        : t->type == T_BITORASSIGN
                        ? BUILTIN_BITORASSIGN
                        : t->type == T_BITXORASSIGN
                        ? BUILTIN_BITXORASSIGN
                        : (assert(false), 0),
                    .first_arg = args.first_child
                }
            }
        });
    }

    return n;
}

static NodeIdx parse_expression(TokenCursor *toks) {
    return parse_assignment_expression(toks);
}

/*
 * Insert an assignment builting at the beginning of `list_expr`,
 * assigning `ident` = `value`
 */
static void insert_assignment(NodeIdx list_expr, const Token *ident, NodeIdx value) {
    ChildCursor args = ChildCursor_init();
    AstNode *v = get_node(value);

    assert(ident->type == T_IDENT);
    assert(v->type == AST_EXPR);
    assert(get_node(list_expr)->type == AST_EXPR && get_node(list_expr)->expr.type == EXPR_LIST);

    NodeIdx ident_expr = alloc_node();
    set_node(ident_expr, &(AstNode) {
        .type = AST_EXPR,
        .start_token = ident,
        .expr = {
            .type = EXPR_IDENT,
            .ident = ident->ident
        }
    });

    ChildCursor_append(&args, ident_expr);
    ChildCursor_append(&args, value);
    
    NodeIdx assignment = alloc_node();
    set_node(assignment, &(AstNode) {
        .type = AST_EXPR,
        .start_token = ident,
        .expr = {
            .type = EXPR_BUILTIN,
            .builtin = {
                .first_arg = args.first_child,
                .op = BUILTIN_ASSIGN
            }
        }
    });

    // need a void literal after the assignment, so it does not
    // alter the value of the local scope
    NodeIdx void_lit = alloc_node();
    set_node(void_lit, &(AstNode) {
        .type = AST_EXPR,
        .start_token = ident,
        .expr = {
            .type = EXPR_LITERAL,
            .literal = {
                .type = LIT_VOID
            }
        }
    });

    // insert the assignment into the list expression
    get_node(assignment)->next_sibling = void_lit;
    get_node(void_lit)->next_sibling = get_node(list_expr)->expr.list.first_child;
    get_node(list_expr)->expr.list.first_child = assignment;
}

static NodeIdx parse_localscope_expression(TokenCursor *toks, enum TokType terminator) {
    if (tok_peek(toks, 0)->type == T_VAR) {
        const Token *start_token = chomp(toks, T_VAR);
        const Token *name = chomp(toks, T_IDENT);
        TypeId type = TYPE_UNKNOWN;
        if (tok_peek(toks, 0)->type == T_COLON) {
            chomp(toks, T_COLON);
            type = parse_type(toks);
        }
        NodeIdx value = 0;
        if (tok_peek(toks, 0)->type == T_ASSIGN) {
            chomp(toks, T_ASSIGN);
            value = parse_expression(toks);
        }
        chomp(toks, T_SEMICOLON);
        NodeIdx scoped_expr = parse_list_expression(toks, terminator);

        if (value) {
            insert_assignment(scoped_expr, name, value);
        }

        NodeIdx scope = alloc_node();
        set_node(scope, &(AstNode) {
            .type = AST_EXPR,
            .start_token = start_token,
            .expr = {
                .type = EXPR_LOCAL_SCOPE,
                .local_scope = {
                    .var_name = name->ident,
                    .var_type = type,
                    .value = value,
                    .scoped_expr = scoped_expr,
                }
            }
        });
        return scope;
    } else {
        return parse_expression(toks);
    }
}

static NodeIdx parse_list_expression(TokenCursor *toks, enum TokType terminator) {
    ChildCursor exprs = ChildCursor_init();
    
    const Token *start_token = tok_peek(toks, 0);
    bool is_void = false;

    while (tok_peek(toks, 0)->type != terminator) {
        ChildCursor_append(&exprs, parse_localscope_expression(toks, terminator));
        is_void = false;
        if (tok_peek(toks, 0)->type == T_SEMICOLON) {
            chomp(toks, T_SEMICOLON);
            is_void = true;
        } else {
            break;
        }
    }

    if (is_void) {
        // expression list ends with semicolon: insert void literal
        NodeIdx void_node = alloc_node();
        set_node(void_node, &(AstNode) {
            .type = AST_EXPR,
            .start_token = start_token,
            .expr = {
                .type = EXPR_LITERAL,
                .literal = {
                    .type = LIT_VOID
                }
            }
        });

        ChildCursor_append(&exprs, void_node);
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

static TypeId parse_type(TokenCursor *toks) {
    const Token *t = tok_next(toks);

    if (t->type == T_IDENT) {
        TypeId type = lookup_type(t->ident);
        if (type == -1) {
            fatal_error(t, "Unknown type '%.*s'", (int)t->ident.len, t->ident.s);
        }
        return type;
    }
    else if (t->type == T_AMPERSAND) {
        // pointer
        TypeId ref = parse_type(toks);
        return make_ptr_type(ref);
    }
    else if (t->type == T_LSQBRACKET) {
        // an array
        TypeId contained = parse_type(toks);
        chomp(toks, T_SEMICOLON);
        const Token *size = tok_next(toks);
        if (size->type != T_LITERAL_U8 &&
            size->type != T_LITERAL_U16 &&
            size->type != T_LITERAL_ANY_INT) {
            parse_error("Expected array length", T_LITERAL_U16, size);
        }
        chomp(toks, T_RSQBRACKET);
        return make_array_type(size->int_literal, contained);
    }
    else {
        parse_error("Expected variable name", T_IDENT, t);
    }
}

static NodeIdx parse_var_def(TokenCursor *toks, bool is_const) {
    NodeIdx var = alloc_node();

    // expect variable name
    const Token *t = tok_next(toks);

    if (t->type != T_IDENT) {
        parse_error("Expected variable name", T_IDENT, t);
    }

    TypeId type = TYPE_UNKNOWN;
    if (tok_peek(toks, 0)->type == T_COLON) {
        chomp(toks, T_COLON);
        type = parse_type(toks);
    }

    NodeIdx value = 0;
    if (tok_peek(toks, 0)->type == T_ASSIGN) {
        chomp(toks, T_ASSIGN);
        value = parse_unary_expression(toks);
    } else if (is_const) {
        fatal_error(tok_peek(toks, 0), "Expected assignment to constant");
    }

    chomp(toks, T_SEMICOLON);

    set_node(var, &(AstNode) {
        .type = AST_DEF_VAR,
        .start_token = t,
        .var_def = {
            .name = t->ident,
            .type = type,
            .is_const = is_const,
            .value = value
        }
    });

    return var;
}

static NodeIdx parse_function(TokenCursor *toks) {
    NodeIdx fn = alloc_node();

    // expect function name
    const Token *t = tok_next(toks);

    if (t->type != T_IDENT) {
        parse_error("Expected function name", T_IDENT, t);
    }

    chomp(toks, T_LPAREN);

    Vec arg_types = vec_init(sizeof(TypeId));
    ChildCursor args = ChildCursor_init();
    {
        while (tok_peek(toks, 0)->type == T_IDENT) {
            NodeIdx a = alloc_node();
            AstNode *n = get_node(a);
            n->type = AST_FN_ARG;
            n->start_token = chomp(toks, T_IDENT);
            n->fn_arg.name = n->start_token->ident;
            chomp(toks, T_COLON);
            n->fn_arg.type = parse_type(toks);

            ChildCursor_append(&args, a);
            vec_push(&arg_types, &n->fn_arg.type);

            if (!check(toks, T_COMMA)) break;
            chomp(toks, T_COMMA);
        }
    }

    chomp(toks, T_RPAREN);

    TypeId ret; // return type
    if (check(toks, T_RARROW)) {
        chomp(toks, T_RARROW);

        ret = parse_type(toks);
    } else {
        ret = VOID;
    }

    // make a type for this function
    TypeId type = add_type((Type) {
        .name = { .s = "fn(..)", .len = 6 }, // XXX generate type name
        .size = 0,
        .stack_size = 0,
        .stack_offset = 0,
        .type = TT_FUNC,
        .func = {
            .args = arg_types,
            .ret = ret
        }
    });

    NodeIdx body = 0;

    if (tok_peek(toks, 0)->type == T_SEMICOLON) {
        // function extern declaration (without body)
        chomp(toks, T_SEMICOLON);
    } else {
        chomp(toks, T_LBRACE);
        body = parse_list_expression(toks, T_RBRACE);
        chomp(toks, T_RBRACE);
    }

    set_node(fn, &(AstNode) {
        .type = AST_FN,
        .start_token = t,
        .fn = {
            .name = t->ident,
            .first_arg = args.first_child,
            .body = body,
            .type = type
        }
    });

    return fn;
}

static void collect_symbols(Program *prog) {
    AstNode *root_node = get_node(prog->root);
    assert(root_node->type == AST_MODULE);

    for (NodeIdx node=root_node->module.first_child; node != 0; node=get_node(node)->next_sibling) {
        AstNode *n = get_node(node);
        if (n->type == AST_FN) {
            if (lookup_program_symbol(prog, n->fn.name) != NULL) {
                fatal_error(n->start_token, "Duplicate definition of symbol %.*s",
                        n->fn.name.len, n->fn.name.s);
            }
            vec_push(&prog->symbols, &(Symbol) {
                .name = n->fn.name,
                .obj = node,
                .type = n->fn.type
            });
        }
        else if (n->type == AST_DEF_VAR) {
            if (lookup_program_symbol(prog, n->var_def.name) != NULL) {
                fatal_error(n->start_token, "Duplicate definition of symbol %.*s",
                        n->var_def.name.len, n->var_def.name.s);
            }
            vec_push(&prog->symbols, &(Symbol) {
                .name = n->var_def.name,
                .obj = node,
                .type = n->var_def.type
            });
        }
        else if (n->type == AST_EXPR && n->expr.type == EXPR_ASM) {
            // fine
        } else {
            assert(false);
        }
    }
}

static void parse_module_body(TokenCursor *toks, ChildCursor *children);
/*
 * Insert the AST of this file into module_children.
 */
static void parse_include(ChildCursor *module_children, const Token *filename) {
    // dammit. Str to char*
    assert(filename->type == T_LITERAL_STR);
    char *filename_ = Str_to_malloced_cstr(filename->str_literal);

    FILE *f = fopen(filename_, "r");
    if (f == NULL) {
        fatal_error(filename, "'%s' not found", filename_);
    }
    // XXX never freed since AST references its memory
    Str buf = read_file(f);
    fclose(f);
    // XXX never freed for same reason
    Vec token_vec = lex(buf, filename_);

    TokenCursor toks = { .tokens = token_vec, .next = 0 };
    parse_module_body(&toks, module_children);

    // XXX Can't free since Tokens refer to filename
    //free(filename_);
}

static void parse_module_body(TokenCursor *toks, ChildCursor *children)
{
    const Token *t;
    for (;;) {
        t = tok_next(toks);

        switch (t->type) {
            case T_CONST:
                ChildCursor_append(children, parse_var_def(toks, true));
                break;
            case T_VAR:
                ChildCursor_append(children, parse_var_def(toks, false));
                break;
            case T_FN:
                ChildCursor_append(children, parse_function(toks));
                break;
            case T_IDENT:
                if (Str_eq(t->ident, "asm")) {
                    ChildCursor_append(children, parse_asm_expression(toks));
                    chomp(toks, T_SEMICOLON);
                } else if (Str_eq(t->ident, "include")) {
                    const Token *filename = chomp(toks, T_LITERAL_STR);
                    chomp(toks, T_SEMICOLON);
                    parse_include(children, filename);
                } else {
                    goto error;
                }
                break;
            case T_EOF:
                return;
            default:
                goto error;
        }
    }

error:
    parse_error("Expected function or end of file", T_FN, t);
}

NodeIdx parse_module(TokenCursor *toks) {
    NodeIdx mod = alloc_node();
    ChildCursor children = ChildCursor_init();

    const Token *t = tok_peek(toks, 0);

    parse_module_body(toks, &children);
    
    set_node(mod, &(AstNode) {
        .type = AST_MODULE,
        .start_token = t,
        .module = {
            .first_child = children.first_child
        }
    });

    return mod;

}

void dump_tokens(Vec *tokens) {
    for (int i=0; i<tokens->len; ++i) {
        printf("%s ", token_type_cstr(((Token*)vec_get(tokens, i))->type));
    }
    printf("\n");
}

void parse_file(Program *prog, const char *filename) {
    //printf("opening %s\n", filename);
    FILE *f = fopen(filename, "r");
    if (f == NULL) {
        fprintf(stderr, "'%s' not found\n", filename);
        exit(-1);
    }
    // XXX never freed since AST references its memory
    Str buf = read_file(f);
    fclose(f);
    // XXX never freed for same reason
    Vec token_vec = lex(buf, filename);
    //dump_tokens(&token_vec);

    TokenCursor toks = { .tokens = token_vec, .next = 0 };
    prog->root = parse_module(&toks);
    collect_symbols(prog);

    /*
    printf("%d KiB of input code\n", buf.len/1024);
    printf("%ld KiB in %ld tokens\n", sizeof(Token)*token_vec.len/1024, token_vec.len);
    printf("%ld KiB in %ld AST tree nodes\n", sizeof(AstNode)*_node_alloc.len/1024,
            _node_alloc.len);
            */
}

static void _indent(int depth) {
    for (int i=0; i<depth*2; ++i) { putchar(' '); }
}

int ast_node_sibling_size(NodeIdx n) {
    if (n == 0) return 0;
    else return 1 + ast_node_sibling_size(get_node(n)->next_sibling);
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
            {
                const Type *type = get_type(node->fn_arg.type);
                fprintf(stdout, "%.*s: %.*s",
                        (int)node->fn_arg.name.len, node->fn_arg.name.s,
                        (int)type->name.len, type->name.s);
            }
            break;
        case AST_FN:
            {
                _indent(depth);
                printf("(%.*s) ",
                        (int)get_type(node->fn.type)->name.len,
                        get_type(node->fn.type)->name.s);
                _indent(depth);
                printf("fn ");
                Str_puts(node->fn.name, stdout);
                printf("(");
                for (NodeIdx child=node->fn.first_arg; child != 0; child = get_node(child)->next_sibling) {
                    print_ast(child, depth+1);
                    fputs(", ", stdout);
                }
                const Type *ret = get_type(get_type(node->fn.type)->func.ret);
                printf(") -> %.*s\n", (int)ret->name.len, ret->name.s);
                if (node->fn.body != 0) {
                    print_ast(node->fn.body, depth+1);
                } else {
                    _indent(depth+1);
                    printf("<fwd decl>\n");
                }
            }
            break;
        case AST_DEF_VAR:
            _indent(depth);
            {
                const Type *type = get_type(node->var_def.type);
                printf("global var %.*s: %.*s\n",
                        (int)node->var_def.name.len, node->var_def.name.s,
                        (int)type->name.len, type->name.s);
            }
            break;
        case AST_EXPR:
            _indent(depth+1);
            printf("(expr node %d: %.*s) ",
                    nidx,
                    (int)get_type(node->expr.eval_type)->name.len,
                    get_type(node->expr.eval_type)->name.s);
            switch (node->expr.type) {
                case EXPR_GOTO:
                    printf("%s (goto node %d)\n", node->expr.goto_.is_continue ? "continue" : "break", node->expr.goto_.target);
                    break;
                case EXPR_ASM:
                    printf("inline asm\n");
                    break;
                case EXPR_LOCAL_SCOPE:
                    {
                        const Type *type = get_type(node->expr.local_scope.var_type);
                        printf("localscope var %.*s: %.*s\n",
                                (int)node->expr.local_scope.var_name.len, node->expr.local_scope.var_name.s,
                                (int)type->name.len, type->name.s);
                        print_ast(node->expr.local_scope.scoped_expr, depth+1);
                    }
                    break;
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
                    switch (node->expr.literal.type) {
                        case LIT_U8:
                            printf("literal u8 (%d)\n", node->expr.literal.literal_int);
                            break;
                        case LIT_U16:
                            printf("literal u16 (%d)\n", node->expr.literal.literal_int);
                            break;
                        case LIT_INT_ANY:
                            printf("literal any int (%d)\n", node->expr.literal.literal_int);
                            break;
                        case LIT_STR:
                            printf("literal str (%.*s)\n", (int)node->expr.literal.literal_str.len, node->expr.literal.literal_str.s);
                            break;
                        case LIT_VOID:
                            printf("literal void\n");
                            break;
                        case LIT_ARRAY:
                            printf("literal array\n");
                            break;
                    }
                    break;
                case EXPR_WHILE_LOOP:
                    printf("while (label %.*s)\n", node->expr.while_loop.label.len, node->expr.while_loop.label.s);
                    _indent(depth+2);
                    printf("condition\n");
                    print_ast(node->expr.while_loop.condition, depth+2);
                    _indent(depth+2);
                    printf("body\n");
                    print_ast(node->expr.while_loop.body, depth+2);
                    break;
                case EXPR_IF_ELSE:
                    printf("if\n");
                    _indent(depth+2);
                    printf("condition\n");
                    print_ast(node->expr.if_else.condition, depth+2);
                    _indent(depth+2);
                    printf("on_true\n");
                    print_ast(node->expr.if_else.on_true, depth+2);
                    _indent(depth+2);
                    if (node->expr.if_else.on_false != 0) {
                        printf("on_false\n");
                        print_ast(node->expr.if_else.on_false, depth+2);
                    }
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
                    printf("expr_builtin (%s)\n", builtin_name(node->expr.builtin.op));
                    _indent(depth+2);
                    printf("args\n");
                    for (NodeIdx arg=node->expr.builtin.first_arg; arg != 0; arg = get_node(arg)->next_sibling) {
                        print_ast(arg, depth+2);
                    }
                    break;
                case EXPR_CAST:
                    {
                        Str name = get_type(node->expr.cast.to_type)->name;
                        printf("cast (as %.*s)\n", (int)name.len, name.s);
                        print_ast(node->expr.cast.arg, depth+2);
                    }
                    break;
            }
            break;
    }
}
