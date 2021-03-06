#include <assert.h>
#include "parser.h"
#include "tokenizer.h"
#include "error.h"
#include "program.h"
#include "slaballoc.h"

static struct SlabAlloc *_node_alloc;

const char* operator_name(enum OperatorOp op) {
    switch (op) {
        case OPERATOR_ADD: return "add";
        case OPERATOR_SUB: return "subtract";
        case OPERATOR_BITAND: return "bitwise and";
        case OPERATOR_BITOR: return "bitwise or";
        case OPERATOR_BITXOR: return "bitwise xor";
        case OPERATOR_LOGICAL_AND: return "logical and";
        case OPERATOR_LOGICAL_OR: return "logical or";
        case OPERATOR_UNARY_LOGICAL_NOT: return "logical not";
        case OPERATOR_MUL: return "multiply";
        case OPERATOR_DIV: return "divide";
        case OPERATOR_MODULO: return "modulo";
        case OPERATOR_ASSIGN: return "assignment";
        case OPERATOR_PLUSASSIGN: return "+=";
        case OPERATOR_MINUSASSIGN: return "-=";
        case OPERATOR_MULASSIGN: return "*=";
        case OPERATOR_DIVASSIGN: return "/=";
        case OPERATOR_MODASSIGN: return "%=";
        case OPERATOR_LSHIFTASSIGN: return "<<=";
        case OPERATOR_RSHIFTASSIGN: return ">>=";
        case OPERATOR_BITANDASSIGN: return "&=";
        case OPERATOR_BITORASSIGN: return "|=";
        case OPERATOR_BITXORASSIGN: return "^=";
        case OPERATOR_EQ: return "equality";
        case OPERATOR_NEQ: return "inequality";
        case OPERATOR_LT: return "less than";
        case OPERATOR_GT: return "greater than";
        case OPERATOR_LTE: return "less than or equal to";
        case OPERATOR_GTE: return "greater than or equal to";
        case OPERATOR_SHIFT_LEFT: return "left shift";
        case OPERATOR_SHIFT_RIGHT: return "right shift";
        case OPERATOR_ARRAY_INDEXING: return "array indexing";
        case OPERATOR_UNARY_NEG: return "unary negation";
        case OPERATOR_UNARY_ADDRESSOF: return "address of";
        case OPERATOR_UNARY_DEREF: return "pointer dereference";
        case OPERATOR_UNARY_BITNOT: return "bitwise not";
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
    _node_alloc = slab_allocator_new(sizeof(AstNode), 0);
    // XXX never freed

    init_types();
}

static NodeIdx alloc_node() {
    AstNode *n = slab_alloc_item(_node_alloc);
    memset(n, 0, sizeof(AstNode));
    return n;
}

static void parse_error(const char *msg, enum TokType expected, const Token *got) __attribute__((noreturn));
static void parse_error(const char *msg, enum TokType expected, const Token *got)
{
    fatal_error(got, "%s: expected %s but found %s", msg, token_type_cstr(expected), token_type_cstr(got->type));
}

AstNode *get_node(NodeIdx idx) { return idx; }
static void set_node(NodeIdx idx, AstNode *n) { *idx = *n; }

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
static NodeIdx parse_localscope_expression(TokenCursor *toks, enum TokType terminator, bool skip_var_keyword, NodeIdx scoped_expr);

// asm("blah")
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

// `my asm here
static NodeIdx parse_asm_literal(const Token *t) {
    assert(t->type == T_ASM);
    NodeIdx expr = alloc_node();
    set_node(expr, &(AstNode) {
        .start_token = t,
        .type = AST_EXPR,
        .expr = {
            .type = EXPR_ASM,
            .asm_ = {
                .asm_text = t->asm_
            }
        }
    });
    return expr;
}

static NodeIdx parse_primary_expression(TokenCursor *toks) {
    const Token *t = tok_next(toks);

    switch (t->type) {
        case T_LITERAL_TRUE:
        case T_LITERAL_FALSE:
            {
                NodeIdx expr = alloc_node();
                set_node(expr, &(AstNode) {
                    .start_token = t,
                    .type = AST_EXPR,
                    .expr = {
                        .type = EXPR_LITERAL,
                        .literal = {
                            .type = LIT_BOOL,
                            .literal_bool = t->type == T_LITERAL_TRUE
                        }
                    }
                });
                return expr;
            }
        case T_LITERAL_U8:
        case T_LITERAL_U16:
        case T_LITERAL_I8:
        case T_LITERAL_I16:
        case T_LITERAL_ANY_INT:
            {
                NodeIdx expr = alloc_node();
                set_node(expr, &(AstNode) {
                    .start_token = t,
                    .type = AST_EXPR,
                    .expr = {
                        .type = EXPR_LITERAL,
                        .literal = {
                            .type = t->type == T_LITERAL_U8
                                ? LIT_U8
                                : t->type == T_LITERAL_I8
                                ? LIT_I8
                                : t->type == T_LITERAL_U16
                                ? LIT_U16
                                : t->type == T_LITERAL_I16
                                ? LIT_I16
                                : t->type == T_LITERAL_ANY_INT
                                ? LIT_INT_ANY
                                : (assert(false), 0),
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
        case T_ASM:
            return parse_asm_literal(t);
        default:
            fatal_error(t, "Expected primary expression but found %s", token_type_cstr(t->type));
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
            NodeIdx arg1 = n;
            NodeIdx arg2 = parse_expression(toks);
            chomp(toks, T_RSQBRACKET);

            n = alloc_node();
            set_node(n, &(AstNode) {
                .type = AST_EXPR,
                .start_token = start_token,
                .expr = {
                    .type = EXPR_FE_OPERATOR,
                    .fe_operator = {
                        .op = OPERATOR_ARRAY_INDEXING,
                        .arg1 = arg1,
                        .arg2 = arg2,
                    }
                }
            });
        }

        // struct member access
        else if (start_token->type == T_PERIOD) {
            chomp(toks, T_PERIOD);
            NodeIdx struct_expr = n;
            Str member_name = chomp(toks, T_IDENT)->ident;

            n = alloc_node();
            set_node(n, &(AstNode) {
                .type = AST_EXPR,
                .start_token = start_token,
                .expr = {
                    .type = EXPR_MEMBER_ACCESS,
                    .member_access = {
                        .struct_expr = struct_expr,
                        .member = member_name
                    }
                }
            });
        }

        else if (start_token->type == T_HASH) {
            chomp(toks, T_HASH);
            NodeIdx arg1 = n;
            n = alloc_node();
            set_node(n, &(AstNode) {
                .type = AST_EXPR,
                .start_token = start_token,
                .expr = {
                    .type = EXPR_FE_OPERATOR,
                    .fe_operator = {
                        .op = OPERATOR_UNARY_DEREF,
                        .arg1 = arg1,
                        .arg2 = 0
                    }
                }
            });
        }

        else if (start_token->type == T_ATSIGN) {
            chomp(toks, T_ATSIGN);
            NodeIdx arg1 = n;
            n = alloc_node();
            set_node(n, &(AstNode) {
                .type = AST_EXPR,
                .start_token = start_token,
                .expr = {
                    .type = EXPR_FE_OPERATOR,
                    .fe_operator = {
                        .op = OPERATOR_UNARY_ADDRESSOF,
                        .arg1 = arg1,
                        .arg2 = 0
                    }
                }
            });
        }

        else if (start_token->type == T_AS) {
            chomp(toks, T_AS);
            TypeId to_type = parse_type(toks);

            NodeIdx arg = n;
            n = alloc_node();
            set_node(n, &(AstNode) {
                .type = AST_EXPR,
                .start_token = start_token,
                .expr = {
                    .type = EXPR_CAST,
                    .cast = {
                        .arg = arg,
                        .to_type = to_type
                    }
                }
            });
        }

        else {
            return n;
        }
    }
}

static NodeIdx parse_unary_expression(TokenCursor *toks) {
    const Token *t = tok_peek(toks, 0);

    if ((t->type == T_MINUS) ||
        (t->type == T_TILDE) ||
        (t->type == T_EXCLAMATION)) {
        tok_next(toks);
        NodeIdx arg1 = parse_unary_expression(toks);

        NodeIdx n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_FE_OPERATOR,
                .fe_operator = {
                    .op = t->type == T_MINUS
                        ? OPERATOR_UNARY_NEG
                        : t->type == T_EXCLAMATION
                        ? OPERATOR_UNARY_LOGICAL_NOT
                        : t->type == T_TILDE
                        ? OPERATOR_UNARY_BITNOT
                        : (assert(false), 0),
                    .arg1 = arg1,
                    .arg2 = 0
                }
            }
        });
        return n;
    } else {
        return parse_postfix_expression(toks);
    }
}

static NodeIdx parse_multiplicative_expression(TokenCursor *toks) {
    NodeIdx n = parse_unary_expression(toks);

    while (tok_peek(toks, 0)->type == T_ASTERISK ||
           tok_peek(toks, 0)->type == T_SLASH ||
           tok_peek(toks, 0)->type == T_PERCENT) {
        const Token *t = tok_next(toks);
        NodeIdx arg1 = n;
        NodeIdx arg2 = parse_unary_expression(toks);

        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_FE_OPERATOR,
                .fe_operator = {
                    .op = t->type == T_ASTERISK
                        ? OPERATOR_MUL
                        : t->type == T_SLASH
                        ? OPERATOR_DIV
                        : t->type == T_PERCENT
                        ? OPERATOR_MODULO
                        : (assert(false), 0),
                    .arg1 = arg1,
                    .arg2 = arg2,
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
        NodeIdx arg1 = n;
        NodeIdx arg2 = parse_multiplicative_expression(toks);
        
        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_FE_OPERATOR,
                .fe_operator = {
                    .op = (
                        t->type == T_PLUS ? OPERATOR_ADD
                        : t->type == T_MINUS ? OPERATOR_SUB
                        : t->type == T_SHIFT_LEFT ? OPERATOR_SHIFT_LEFT
                        : t->type == T_SHIFT_RIGHT ? OPERATOR_SHIFT_RIGHT
                        : t->type == T_AMPERSAND ? OPERATOR_BITAND
                        : t->type == T_PIPE ? OPERATOR_BITOR
                        : t->type == T_ACUTE ? OPERATOR_BITXOR
                        : (assert(false), 0)
                    ),
                    .arg1 = arg1,
                    .arg2 = arg2,
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

        NodeIdx arg1 = n;
        NodeIdx arg2 = parse_additive_expression(toks);
        
        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_FE_OPERATOR,
                .fe_operator = {
                    .op = t->type == T_PIPE ? OPERATOR_LOGICAL_OR : OPERATOR_LOGICAL_AND,
                    .arg1 = arg1,
                    .arg2 = arg2,
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
        NodeIdx arg1 = n;
        NodeIdx arg2 = parse_logical_expression(toks);
        
        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_FE_OPERATOR,
                .fe_operator = {
                    .op = t->type == T_EQ
                          ? OPERATOR_EQ
                          : t->type == T_NEQ
                          ? OPERATOR_NEQ
                          : t->type == T_GT
                          ? OPERATOR_GT
                          : t->type == T_LT
                          ? OPERATOR_LT
                          : t->type == T_GTE
                          ? OPERATOR_GTE
                          : t->type == T_LTE
                          ? OPERATOR_LTE
                          : (assert(false), 0),
                    .arg1 = arg1,
                    .arg2 = arg2,
                }
            }
        });
    }

    return n;
}

static bool is_loop_token(const Token *t) {
    return t->type == T_WHILE || t->type == T_LOOP || t->type == T_FOR;
}

static NodeIdx parse_conditional_expression(TokenCursor *toks) {
    Str label = { .s = 0 };

    if (tok_peek(toks, 0)->type == T_JUMP_LABEL &&
        tok_peek(toks, 1)->type == T_COLON &&
        is_loop_token(tok_peek(toks, 2))) {

        label = chomp(toks, T_JUMP_LABEL)->label;
        chomp(toks, T_COLON);
    }

    if (tok_peek(toks, 0)->type == T_WHILE ||
        tok_peek(toks, 0)->type == T_LOOP) {
        const Token *t = tok_next(toks);

        NodeIdx condition = 0;
        if (t->type == T_WHILE) {
            condition = parse_expression(toks);
        }

        chomp(toks, T_LBRACE);
        NodeIdx body = parse_list_expression(toks, T_RBRACE);
        chomp(toks, T_RBRACE);

        NodeIdx loop = alloc_node();
        set_node(loop, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_LOOP,
                .loop = {
                    .label = label,
                    .condition = condition,
                    .body = body,
                    .on_next_iter = 0
                }
            }
        });

        return loop;
    } else if (tok_peek(toks, 0)->type == T_FOR) {
        const Token *t = chomp(toks, T_FOR);

        NodeIdx loop = alloc_node();
        // to keep assertions in parse_localscope_expression happy
        set_node(loop, &(AstNode) { .type = AST_EXPR });

        NodeIdx loop_scope = 0;
        if (tok_peek(toks, 0)->type != T_SEMICOLON) {
            loop_scope = parse_localscope_expression(toks, T_RBRACE, true, loop);
        }
        chomp(toks, T_SEMICOLON);

        NodeIdx condition = 0;
        if (tok_peek(toks, 0)->type != T_SEMICOLON) {
            condition = parse_expression(toks);
        }
        chomp(toks, T_SEMICOLON);

        NodeIdx on_next_iter = 0;
        if (tok_peek(toks, 0)->type != T_SEMICOLON) {
            on_next_iter = parse_expression(toks);
        }

        chomp(toks, T_LBRACE);
        NodeIdx body = parse_list_expression(toks, T_RBRACE);
        chomp(toks, T_RBRACE);

        set_node(loop, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_LOOP,
                .loop = {
                    .label = label,
                    .condition = condition,
                    .body = body,
                    .on_next_iter = on_next_iter
                }
            }
        });

        return loop_scope ? loop_scope : loop;
    } else if (tok_peek(toks, 0)->type == T_IF) {
        const Token *t = chomp(toks, T_IF);

        NodeIdx condition = parse_expression(toks);
        NodeIdx on_true = parse_expression(toks);
        NodeIdx on_false = 0;

        if (tok_peek(toks, 0)->type == T_ELSE) {
            chomp(toks, T_ELSE);
            on_false = parse_expression(toks);
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

        NodeIdx arg1 = n;
        NodeIdx arg2 = parse_assignment_expression(toks);
        
        n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_FE_OPERATOR,
                .fe_operator = {
                    .op = t->type == T_ASSIGN
                        ? OPERATOR_ASSIGN
                        : t->type == T_PLUSASSIGN
                        ? OPERATOR_PLUSASSIGN
                        : t->type == T_MINUSASSIGN
                        ? OPERATOR_MINUSASSIGN
                        : t->type == T_MULASSIGN
                        ? OPERATOR_MULASSIGN
                        : t->type == T_DIVASSIGN
                        ? OPERATOR_DIVASSIGN
                        : t->type == T_MODASSIGN
                        ? OPERATOR_MODASSIGN
                        : t->type == T_LSHIFTASSIGN
                        ? OPERATOR_LSHIFTASSIGN
                        : t->type == T_RSHIFTASSIGN
                        ? OPERATOR_RSHIFTASSIGN
                        : t->type == T_BITANDASSIGN
                        ? OPERATOR_BITANDASSIGN
                        : t->type == T_BITORASSIGN
                        ? OPERATOR_BITORASSIGN
                        : t->type == T_BITXORASSIGN
                        ? OPERATOR_BITXORASSIGN
                        : (assert(false), 0),
                    .arg1 = arg1,
                    .arg2 = arg2,
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
 * Insert an assignment operator at the beginning of `scoped_expr`,
 * assigning `ident` = `value`
 * Returns NodeIdx of modified scoped_expr
 */
static NodeIdx insert_assignment(NodeIdx scoped_expr, const Token *ident, NodeIdx value) {
    assert(ident->type == T_IDENT);
    assert(get_node(value)->type == AST_EXPR);
    assert(get_node(scoped_expr)->type == AST_EXPR);

    NodeIdx ident_expr = alloc_node();
    set_node(ident_expr, &(AstNode) {
        .type = AST_EXPR,
        .start_token = ident,
        .expr = {
            .type = EXPR_IDENT,
            .ident = ident->ident
        }
    });

    NodeIdx assignment = alloc_node();
    set_node(assignment, &(AstNode) {
        .type = AST_EXPR,
        .start_token = ident,
        .expr = {
            .type = EXPR_FE_OPERATOR,
            .fe_operator = {
                .op = OPERATOR_ASSIGN,
                .arg1 = ident_expr,
                .arg2 = value
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
    NodeIdx list = alloc_node();
    ChildCursor args = ChildCursor_init();
    ChildCursor_append(&args, assignment);
    ChildCursor_append(&args, void_lit);
    ChildCursor_append(&args, scoped_expr);
    set_node(list, &(AstNode) {
        .type = AST_EXPR,
        .start_token = ident,
        .expr = {
            .type = EXPR_LIST,
            .list = {
                .first_child = args.first_child
            }
        }
    });
    return list;
}

/**
 * if `scoped_expr` != 0 then this NodeIdx will be used as the localscope scoped_expr,
 * rather than parsing an expression there.
 */
static NodeIdx parse_localscope_expression(TokenCursor *toks, enum TokType terminator, bool skip_var_keyword, NodeIdx scoped_expr) {
    if (tok_peek(toks, 0)->type == T_VAR || skip_var_keyword) {
        const Token *start_token = tok_peek(toks, 0);
        if (!skip_var_keyword) chomp(toks, T_VAR);
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

        if (tok_peek(toks, 0)->type == T_COMMA) {
            chomp(toks, T_COMMA);
            scoped_expr = parse_localscope_expression(toks, terminator, true, scoped_expr);
        } else if (scoped_expr == 0) {
            scoped_expr = parse_list_expression(toks, terminator);
        }

        if (value) {
            scoped_expr = insert_assignment(scoped_expr, name, value);
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
        ChildCursor_append(&exprs, parse_localscope_expression(toks, terminator, false, 0));
        is_void = false;
        if (tok_peek(toks, 0)->type == T_SEMICOLON) {
            chomp(toks, T_SEMICOLON);
            is_void = true;
        //} else {
            //break;
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
    else if (t->type == T_ATSIGN) {
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
    } else if (t->type == T_FN) {
        // a function type
        chomp(toks, T_LPAREN);
        Vec arg_types = vec_init(sizeof(TypeId));
        while (tok_peek(toks, 0)->type != T_RPAREN) {
            const TypeId arg = parse_type(toks);
            vec_push(&arg_types, &arg);
            if (tok_peek(toks, 0)->type != T_COMMA) {
                break;
            }
            chomp(toks, T_COMMA);
        }
        chomp(toks, T_RPAREN);
        TypeId ret = VOID;
        if (tok_peek(toks, 0)->type == T_RARROW) {
            chomp(toks, T_RARROW);
            ret = parse_type(toks);
        }

        return make_fn_type(&arg_types, ret);
    }
    else {
        parse_error("Expected variable name", T_IDENT, t);
    }
}

static void parse_var_def(TokenCursor *toks, ChildCursor *module_children, bool is_const) {
    for(;;) {
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

        ChildCursor_append(module_children, var);

        if (tok_peek(toks, 0)->type == T_COMMA) {
            chomp(toks, T_COMMA);
            continue;
        }
        else {
            break;
        }
    }
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
    TypeId type = make_fn_type(&arg_types, ret);

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

static TypeId parse_struct_def(TokenCursor *toks) {
    Vec members = vec_init(sizeof(StructMember));
    int offset = 0;

    Str struct_name = chomp(toks, T_IDENT)->ident;
    chomp(toks, T_LBRACE);
    while (tok_peek(toks, 0)->type != T_RBRACE) {
        StructMember m;

        const Token *member_name = chomp(toks, T_IDENT);

        // check name is unique
        for (int i=0; i<members.len; ++i) {
            if (Str_eq2(
                ((StructMember*)vec_get(&members, i))->name,
                member_name->ident)) {
                fatal_error(
                    member_name,
                    "Duplicate struct member `%.*s`",
                    member_name->ident.len,
                    member_name->ident.s);
            }
        }

        chomp(toks, T_COLON);
        m.name = member_name->ident;
        m.type = parse_type(toks);
        m.offset = offset;

        offset += get_type(m.type)->size;

        /*
        printf("Got struct member %.*s type %.*s (offset %d)\n",
                m.name.len,
                m.name.s, 
                get_type(m.type)->name.len,
                get_type(m.type)->name.s,
                m.offset);
                */
        vec_push(&members, &m);
    }
    /*
    printf("Found %d members of struct %.*s (size %d)\n",
            (int)members.len,
            struct_name.len,
            struct_name.s,
            offset);
            */
    chomp(toks, T_RBRACE);

    return make_struct_type(struct_name, &members);
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
                parse_var_def(toks, children, true);
                break;
            case T_VAR:
                parse_var_def(toks, children, false);
                break;
            case T_FN:
                ChildCursor_append(children, parse_function(toks));
                break;
            case T_ASM:
                ChildCursor_append(children, parse_asm_literal(t));
                break;
            case T_IDENT:
                if (Str_eq(t->ident, "asm")) {
                    ChildCursor_append(children, parse_asm_expression(toks));
                } else if (Str_eq(t->ident, "include")) {
                    const Token *filename = chomp(toks, T_LITERAL_STR);
                    parse_include(children, filename);
                } else {
                    goto error;
                }
                break;
            case T_STRUCT:
                parse_struct_def(toks);
                break;
            case T_EOF:
                return;
            default:
                goto error;
        }
    }

error:
    fatal_error(t, "Unexpected token `%s`", token_type_cstr(t->type));
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
            printf("(expr node %p: %.*s) ",
                    nidx,
                    (int)get_type(node->expr.eval_type)->name.len,
                    get_type(node->expr.eval_type)->name.s);
            switch (node->expr.type) {
                case EXPR_GOTO:
                    printf("%s (goto node %p)\n", node->expr.goto_.is_continue ? "continue" : "break", node->expr.goto_.target);
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
                        case LIT_BOOL:
                            printf("literal bool (%s)\n", node->expr.literal.literal_bool ? "true" : "false");
                            break;
                        case LIT_U8:
                            printf("literal u8 (%d)\n", node->expr.literal.literal_int);
                            break;
                        case LIT_U16:
                            printf("literal u16 (%d)\n", node->expr.literal.literal_int);
                            break;
                        case LIT_I8:
                            printf("literal i8 (%d)\n", node->expr.literal.literal_int);
                            break;
                        case LIT_I16:
                            printf("literal i16 (%d)\n", node->expr.literal.literal_int);
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
                case EXPR_LOOP:
                    {
                        bool has_cond = node->expr.loop.condition != 0;
                        printf("%s (label %.*s)\n", has_cond ? "while" : "loop", node->expr.loop.label.len, node->expr.loop.label.s);
                        if (has_cond) {
                            _indent(depth+2);
                            printf("condition\n");
                            print_ast(node->expr.loop.condition, depth+2);
                        }
                        _indent(depth+2);
                        printf("body\n");
                        print_ast(node->expr.loop.body, depth+2);
                    }
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
                    printf("expr_call (%s)\n", node->expr.call.is_indirect ? "indirect" : "direct");
                    print_ast(node->expr.call.callee, depth+1);
                    _indent(depth+2);
                    printf("args\n");
                    for (NodeIdx arg=node->expr.call.first_arg; arg != 0; arg = get_node(arg)->next_sibling) {
                        print_ast(arg, depth+2);
                    }
                    break;
                case EXPR_BUILTIN:
                    printf("builtin (%d)\n", node->expr.builtin.op);
                    _indent(depth+2);
                    printf("args\n");
                    print_ast(node->expr.builtin.arg1, depth+2);
                    if (node->expr.builtin.arg2) {
                        print_ast(node->expr.builtin.arg2, depth+2);
                    }
                    break;
                case EXPR_FE_OPERATOR:
                    printf("expr_operator (%s)\n", operator_name(node->expr.fe_operator.op));
                    _indent(depth+2);
                    printf("args\n");
                    print_ast(node->expr.fe_operator.arg1, depth+2);
                    if (node->expr.fe_operator.arg2) {
                        print_ast(node->expr.fe_operator.arg2, depth+2);
                    }
                    break;
                case EXPR_RETURN:
                    printf("return something\n");
                    break;
                case EXPR_MEMBER_ACCESS:
                    printf("access struct member `%.*s`\n",
                            node->expr.member_access.member.len,
                            node->expr.member_access.member.s);
                    print_ast(node->expr.member_access.struct_expr, depth+2);
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
