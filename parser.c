#include <assert.h>
#include "parser.h"
#include "tokenizer.h"
#include "error.h"
#include "program.h"

static Vec _node_alloc;

static Str read_file(FILE *f) {
    fseek(f, 0, SEEK_END);
    const size_t len = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *buf = malloc(len);
    const size_t num_read = fread(buf, 1, len, f);
    assert(num_read == len);

    return (Str) { buf, len };
}

static const Token *tok_peek(TokenCursor *cursor, int ahead) {
    int next = cursor->next;
    while (next < cursor->tokens.len) {
        const Token *t = (Token*)vec_get(&cursor->tokens, next);
        next++;

        if (ahead-- == 0) {
            return t;
        }
    }
    fprintf(stderr, "tok_peek() called at EOF position\n");
    fflush(stderr);
    abort();
}

static const Token *tok_next(TokenCursor *cursor) {
    while (cursor->next < cursor->tokens.len) {
        const Token *t = (Token*)vec_get(&cursor->tokens, cursor->next);
        cursor->next++;

        return t;
    }
    fprintf(stderr, "tok_next() called at EOF position\n");
    fflush(stderr);
    abort();
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
    fatal_error(got, "%s (expected %s but found %s)", msg, token_type_cstr(expected), token_type_cstr(got->type));
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
static NodeIdx parse_list_expression(TokenCursor *toks);

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
                        .type = EXPR_LITERAL_U8,
                        .literal_int = t->int_literal
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
                        .type = EXPR_LITERAL_U16,
                        .literal_int = t->int_literal
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
                        .type = EXPR_LITERAL_STR,
                        .literal_str = t->str_literal
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
                NodeIdx expr = parse_list_expression(toks);
                chomp(toks, T_RPAREN);
                return expr;
            }
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
            ChildCursor_append(&args, parse_list_expression(toks));
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
        else {
            return n;
        }
    }
}

static NodeIdx parse_unary_expression(TokenCursor *toks) {
    const Token *t = tok_peek(toks, 0);

    if (t->type == T_MINUS) {
        chomp(toks, T_MINUS);
        ChildCursor args = ChildCursor_init();
        ChildCursor_append(&args, parse_postfix_expression(toks));

        NodeIdx n = alloc_node();
        set_node(n, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_BUILTIN,
                .builtin = {
                    .op = BUILTIN_UNARY_NEG,
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

    while (tok_peek(toks, 0)->type == T_ASTERISK) {
        const Token *start_token = chomp(toks, T_ASTERISK);

        ChildCursor args = ChildCursor_init();
        ChildCursor_append(&args, n);
        ChildCursor_append(&args, parse_cast_expression(toks));

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

    while (tok_peek(toks, 0)->type == T_PLUS ||
           tok_peek(toks, 0)->type == T_MINUS ||
           tok_peek(toks, 0)->type == T_AMPERSAND ||
           tok_peek(toks, 0)->type == T_PIPE ||
           tok_peek(toks, 0)->type == T_ACUTE
    ) {
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
                        : t->type == T_AMPERSAND ? BUILTIN_BITAND
                        : t->type == T_PIPE ? BUILTIN_BITOR
                        : t->type == T_ACUTE ? BUILTIN_BITXOR
                        : (abort(), BUILTIN_ADD)
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

    while (tok_peek(toks, 0)->type == T_LOGICAL_OR ||
           tok_peek(toks, 0)->type == T_LOGICAL_AND
    ) {
        const Token *t = tok_next(toks);

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
                    .op = t->type == T_LOGICAL_OR ? BUILTIN_LOGICAL_OR : BUILTIN_LOGICAL_AND,
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
           tok_peek(toks, 0)->type == T_NEQ
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
                    .op = t->type == T_EQ ? BUILTIN_EQ : BUILTIN_NEQ,
                    .first_arg = args.first_child
                }
            }
        });
    }

    return n;
}

static NodeIdx parse_conditional_expression(TokenCursor *toks) {
    if (tok_peek(toks, 0)->type == T_WHILE) {
        const Token *t = chomp(toks, T_WHILE);
        NodeIdx condition = parse_expression(toks);
        chomp(toks, T_LBRACE);
        NodeIdx body = parse_list_expression(toks);
        chomp(toks, T_RBRACE);

        NodeIdx while_loop = alloc_node();
        set_node(while_loop, &(AstNode) {
            .type = AST_EXPR,
            .start_token = t,
            .expr = {
                .type = EXPR_WHILE_LOOP,
                .while_loop = {
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
        NodeIdx on_true = parse_list_expression(toks);
        chomp(toks, T_RBRACE);
        NodeIdx on_false = 0;

        if (tok_peek(toks, 0)->type == T_ELSE) {
            chomp(toks, T_ELSE);
            chomp(toks, T_LBRACE);
            on_false = parse_list_expression(toks);
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
    if (tok_peek(toks, 0)->type == T_ASSIGN)
    {
        const Token *t = chomp(toks, T_ASSIGN);

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
                    .op = BUILTIN_ASSIGN,
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

static NodeIdx parse_localscope_expression(TokenCursor *toks) {
    if (tok_peek(toks, 0)->type == T_VAR) {
        const Token *start_token = chomp(toks, T_VAR);
        const Token *name = chomp(toks, T_IDENT);
        chomp(toks, T_COLON);
        TypeId type = parse_type(toks);
        chomp(toks, T_SEMICOLON);
        NodeIdx scoped_expr = parse_list_expression(toks);

        NodeIdx scope = alloc_node();
        set_node(scope, &(AstNode) {
            .type = AST_EXPR,
            .start_token = start_token,
            .expr = {
                .type = EXPR_LOCAL_SCOPE,
                .local_scope = {
                    .var_name = name->ident,
                    .var_type = type,
                    .scoped_expr = scoped_expr,
                }
            }
        });
        return scope;
    } else {
        return parse_expression(toks);
    }
}

static NodeIdx parse_list_expression(TokenCursor *toks) {
    ChildCursor exprs = ChildCursor_init();
    
    const Token *start_token = tok_peek(toks, 0);
    bool is_void = false;

    while (tok_peek(toks, 0)->type != T_RBRACE) {
        ChildCursor_append(&exprs, parse_localscope_expression(toks));
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
            .expr = { .type = EXPR_LITERAL_VOID }
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
    else if (t->type == T_LSQBRACKET) {
        // an array
        TypeId contained = parse_type(toks);
        chomp(toks, T_SEMICOLON);
        const Token *size = tok_next(toks);
        if (size->type != T_LITERAL_U8 && size->type != T_LITERAL_U16) {
            parse_error("Expected array length", T_LITERAL_U16, size);
        }
        chomp(toks, T_RSQBRACKET);
        const int byte_size = get_type(contained)->size * size->int_literal;

        char buf[256];
        snprintf(buf, sizeof(buf), "[%.*s; %d]",
                (int)get_type(contained)->name.len,
                get_type(contained)->name.s,
                size->int_literal);
        // XXX this is never deallocated (but the compiler has no teardown anyhow...)
        char *type_name = strdup(buf);

        return add_type((Type) {
            .type = TT_ARRAY,
            .name = { .s = type_name, .len = strlen(type_name) },
            .size = byte_size,
            .stack_size = byte_size,
            .stack_offset = 0,
            .array = {
                .contained = contained
            }
        });
    }
    else {
        parse_error("Expected variable name", T_IDENT, t);
    }
}

static NodeIdx parse_var_def(TokenCursor *toks) {
    NodeIdx var = alloc_node();

    // expect variable name
    const Token *t = tok_next(toks);

    if (t->type != T_IDENT) {
        parse_error("Expected variable name", T_IDENT, t);
    }

    chomp(toks, T_COLON);
    TypeId type = parse_type(toks);
    chomp(toks, T_SEMICOLON);

    set_node(var, &(AstNode) {
        .type = AST_DEF_VAR,
        .start_token = t,
        .var_def = {
            .name = t->ident,
            .type = type,
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
        body = parse_list_expression(toks);
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
            vec_push(&prog->symbols, &(Symbol) {
                .name = n->fn.name,
                .obj = node,
                .type = n->fn.type
            });
        }
        else if (n->type == AST_DEF_VAR) {
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

    free(filename_);
}

static void parse_module_body(TokenCursor *toks, ChildCursor *children)
{
    const Token *t;
    for (;;) {
        t = tok_next(toks);

        switch (t->type) {
            case T_VAR:
                ChildCursor_append(children, parse_var_def(toks));
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

    TokenCursor toks = { .tokens = token_vec, .next = 0 };
    prog->root = parse_module(&toks);
    collect_symbols(prog);
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
            printf("(%.*s) ",
                    (int)get_type(node->expr.eval_type)->name.len,
                    get_type(node->expr.eval_type)->name.s);
            switch (node->expr.type) {
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
                case EXPR_LITERAL_U8:
                    printf("literal u8 (%d)\n", node->expr.literal_int);
                    break;
                case EXPR_LITERAL_U16:
                    printf("literal u16 (%d)\n", node->expr.literal_int);
                    break;
                case EXPR_LITERAL_STR:
                    printf("literal str (%.*s)\n", (int)node->expr.literal_str.len, node->expr.literal_str.s);
                    break;
                case EXPR_LITERAL_VOID:
                    printf("literal void\n");
                    break;
                case EXPR_WHILE_LOOP:
                    printf("while\n");
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
                    printf("expr_builtin (op=%d)\n", node->expr.builtin.op);
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
