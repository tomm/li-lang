#include "program.h"
#include "error.h"
#include <assert.h>

Symbol *lookup_program_symbol(Program *prog, Str name) {
    for (int i=0; i<prog->symbols.len; ++i) {
        Symbol *sym = vec_get(&prog->symbols, i);
        if (Str_eq2(sym->name, name)) return sym;
    }
    return NULL;
}

/* Crud for function local scopes */
typedef struct Variable {
    Str name;
    TypeId type;
} Variable;

typedef struct JumpLabel {
    Str label;
    NodeIdx node;
} JumpLabel;

typedef struct Scope {
    Vec /*<Variable>*/ vars;
    Vec /*<JumpLabel>*/ labels;
} Scope;
static Scope new_localscope() {
    return (Scope){
        .vars = vec_init(sizeof(Variable)),
        .labels = vec_init(sizeof(JumpLabel)),
    };
}
static void free_localscope(Scope *scope) { vec_free(&scope->vars); }
static void scope_push(Scope *scope, Variable var) { vec_push(&scope->vars, &var); }
static void scope_pop(Scope *scope) { vec_pop(&scope->vars, NULL); }
static Variable *scope_lookup(Scope *scope, Str name) {
    for (int i=0; i<scope->vars.len; ++i) {
        Variable *var = vec_get(&scope->vars, i);
        if (Str_eq2(var->name, name)) return var;
    }
    return NULL;
}
static void label_push(Scope *scope, JumpLabel l) { vec_push(&scope->labels, &l); }
static void label_pop(Scope *scope) { vec_pop(&scope->labels, NULL); }
static JumpLabel *label_lookup(Scope *scope, Str *name) {
    for (int i=scope->labels.len-1; i>=0; --i) {
        JumpLabel *l = vec_get(&scope->labels, i);
        // if name is empty (ie break/continue without label) resolve to
        // most recently scoped label
        if (name->s == NULL) {
            return l;
        }
        if (Str_eq2(*name, l->label)) {
            return l;
        }
    }
    return NULL;
}

static TypeId typecheck_expr(Program *prog, Scope *scope, NodeIdx expr);

typedef struct ValidBuiltin {
    enum BuiltinOp op;
    enum TypeType arg1;
    enum TypeType arg2;
    TypeId ret;
} ValidBuiltin;

static const ValidBuiltin valid_builtins[] = {
    { BUILTIN_UNARY_ADDRESSOF, -1 /* any */, TT_PRIM_VOID, -1 },
    { BUILTIN_UNARY_DEREF, TT_PTR, TT_PRIM_VOID, -1 /* any */ },
    { BUILTIN_ASSIGN, -1, -1, -1 },
    { BUILTIN_ARRAY_INDEXING, TT_ARRAY, TT_PRIM_U8, -1 /* any */ },
    { BUILTIN_ARRAY_INDEXING, TT_ARRAY, TT_PRIM_U16, -1 /* any */ },
    { BUILTIN_ADD, TT_PTR, TT_PRIM_U16, -1 },
    { BUILTIN_SUB, TT_PTR, TT_PRIM_U16, -1 },
    { BUILTIN_PLUSASSIGN, TT_PTR, TT_PRIM_U16, -1 },
    { BUILTIN_MINUSASSIGN, TT_PTR, TT_PRIM_U16, -1 },

    { BUILTIN_UNARY_NEG, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_SHIFT_LEFT, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_SHIFT_RIGHT, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_ADD, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_SUB, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_NEQ, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_EQ, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_GT, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_LT, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_GTE, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_LTE, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_MUL, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_DIV, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_MODULO, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_BITXOR, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_BITAND, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_BITOR, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_PLUSASSIGN, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_MINUSASSIGN, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_MULASSIGN, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_DIVASSIGN, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_MODASSIGN, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_LSHIFTASSIGN, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_RSHIFTASSIGN, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_BITANDASSIGN, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_BITORASSIGN, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_BITXORASSIGN, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_ASSIGN, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_UNARY_NEG, TT_PRIM_U8, TT_PRIM_VOID, U8 },
    { BUILTIN_UNARY_BITNOT, TT_PRIM_U8, TT_PRIM_VOID, U8 },
    { BUILTIN_UNARY_LOGICAL_NOT, TT_PRIM_U8, TT_PRIM_VOID, U8 },

    { BUILTIN_SHIFT_LEFT, TT_PRIM_U16, TT_PRIM_U8, U16 },
    { BUILTIN_SHIFT_RIGHT, TT_PRIM_U16, TT_PRIM_U8, U16 },
    { BUILTIN_ADD, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_SUB, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_NEQ, TT_PRIM_U16, TT_PRIM_U16, U8 },
    { BUILTIN_EQ, TT_PRIM_U16, TT_PRIM_U16, U8 },
    { BUILTIN_GT, TT_PRIM_U16, TT_PRIM_U16, U8 },
    { BUILTIN_LT, TT_PRIM_U16, TT_PRIM_U16, U8 },
    { BUILTIN_GTE, TT_PRIM_U16, TT_PRIM_U16, U8 },
    { BUILTIN_LTE, TT_PRIM_U16, TT_PRIM_U16, U8 },
    { BUILTIN_MUL, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_DIV, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_MODULO, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_BITXOR, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_BITAND, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_BITOR, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_PLUSASSIGN, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_MINUSASSIGN, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_MULASSIGN, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_DIVASSIGN, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_MODASSIGN, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_LSHIFTASSIGN, TT_PRIM_U16, TT_PRIM_U8, U16 },
    { BUILTIN_RSHIFTASSIGN, TT_PRIM_U16, TT_PRIM_U8, U16 },
    { BUILTIN_BITANDASSIGN, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_BITORASSIGN, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_BITXORASSIGN, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_UNARY_NEG, TT_PRIM_U16, TT_PRIM_VOID, U16 },
    { BUILTIN_UNARY_BITNOT, TT_PRIM_U16, TT_PRIM_VOID, U16 },
    
    { BUILTIN_LOGICAL_AND, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_LOGICAL_OR, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { -1 }
};

static TypeId typecheck_builtin(Program *prog, Scope *scope, NodeIdx expr) {
    AstNode *n = get_node(expr);
    enum BuiltinOp op = n->expr.builtin.op;

    const int num_args = ast_node_sibling_size(n->expr.builtin.first_arg);
    assert(num_args == 1 || num_args == 2);

    NodeIdx arg1 = n->expr.builtin.first_arg;
    NodeIdx arg2 = get_node(arg1)->next_sibling;

    TypeId t1 = typecheck_expr(prog, scope, arg1);
    TypeId t2 = num_args == 2 ? typecheck_expr(prog, scope, arg2) : VOID;

    enum TypeType tt1 = get_type(t1)->type;
    enum TypeType tt2 = t2 ? get_type(t2)->type : TT_PRIM_VOID;

    // do the argument types match a valid builtin?
    for (int i=0; valid_builtins[i].op != -1; ++i) {
        const ValidBuiltin *v = &valid_builtins[i];
        if (v->op == op &&
            (v->arg1 == -1 || v->arg1 == tt1) &&
            (v->arg2 == -1 || v->arg2 == tt2)) {
            if (v->ret == -1) {
                // special handling of return type
                switch (op) {
                    case BUILTIN_UNARY_ADDRESSOF:
                        return make_ptr_type(t1);
                    case BUILTIN_UNARY_DEREF:
                        return get_type(t1)->ptr.ref;
                    case BUILTIN_ADD:
                    case BUILTIN_SUB:
                        if (tt1 == TT_PTR) return t1;
                        else assert(false);
                    case BUILTIN_ASSIGN:
                        if (!is_type_eq(t1, t2)) {
                            goto error;
                        }
                        return t1;
                    case BUILTIN_MINUSASSIGN:
                        // ptr -= u16
                        return t1;
                    case BUILTIN_PLUSASSIGN:
                        // ptr += u16
                        return t1;
                    case BUILTIN_ARRAY_INDEXING:
                        return get_type(t1)->array.contained;
                    default: assert(false);
                }
            } else {
                return v->ret;
            }
        }
    }
error:
    fatal_error(n->start_token, "invalid arguments to operator %s: '%.*s' and '%.*s'",
            builtin_name(op),
            (int)get_type(t1)->name.len,
            get_type(t1)->name.s,
            (int)get_type(t2)->name.len,
            get_type(t2)->name.s);
}


// XXX also resolves gotos. why do we do both? because AST isn't simple
// to traverse...
static TypeId typecheck_expr(Program *prog, Scope *scope, NodeIdx expr) {
    AstNode *n = get_node(expr);
    TypeId t = TYPE_UNKNOWN;
    switch (n->expr.type) {
        case EXPR_GOTO:
            {
                if (scope->labels.len == 0) {
                    fatal_error(n->start_token, "Break/continue used outside of loop");
                }
                JumpLabel *l = label_lookup(scope, &n->expr.goto_.label);
                if (l == NULL) {
                    fatal_error(n->start_token, "Unknown label '%.*s'",
                            n->expr.goto_.label.len, n->expr.goto_.label.s);
                }
                n->expr.goto_.target = l->node;
            }
            t = VOID;
            break;
        case EXPR_ASM:
            t = VOID;
            break;
        case EXPR_CAST:
            {
                TypeId from_type = typecheck_expr(prog, scope, n->expr.cast.arg);
                t = n->expr.cast.to_type;

                // valid casts in li
                if (is_type_eq(from_type, t) ||
                    (from_type == U8 && t == U16) ||
                    (from_type == U16 && t == U8) ||
                    (get_type(t)->type == TT_PTR && from_type == U16) ||
                    (get_type(from_type)->type == TT_PTR && t == U16) ||
                    (get_type(from_type)->type == TT_PTR && get_type(t)->type == TT_PTR)
                ) {
                    // fine
                } else {
                    fatal_error(n->start_token, "Invalid type cast (from %.*s to %.*s)",
                            (int)get_type(from_type)->name.len,
                            get_type(from_type)->name.s,
                            (int)get_type(t)->name.len,
                            get_type(t)->name.s);
                }
            }
            break;
        case EXPR_LIST:
            {
                t = VOID;
                for (NodeIdx e=n->expr.list.first_child; e != 0; e=get_node(e)->next_sibling) {
                    t = typecheck_expr(prog, scope, e);
                }
            }
            break;
        case EXPR_IDENT:
            {
                Variable *var = scope_lookup(scope, n->expr.ident);
                if (var) {
                    t = var->type;
                } else {
                    Symbol *sym = lookup_program_symbol(prog, n->expr.ident);
                    if (sym) {
                        t = sym->type;
                    } else {
                        fatal_error(n->start_token, "Use of undeclared identifier '%.*s'",
                                (int)n->expr.ident.len, n->expr.ident.s);
                    }
                }
            }
            break;
        case EXPR_LITERAL:
            switch (n->expr.literal.type) {
                case LIT_U8:
                    t = U8;
                    break;
                case LIT_U16:
                    t = U16;
                    break;
                case LIT_VOID:
                    t = VOID;
                    break;
                case LIT_STR:
                    // len+1 to accommodate null terminator
                    t = make_array_type(n->expr.literal.literal_str.len+1, U8);
                    break;
                case LIT_ARRAY:
                    {
                        NodeIdx item = n->expr.literal.literal_array_first_val;
                        if (item == 0) {
                            fatal_error(n->start_token, "Zero length array literal not permitted");
                        }
                        t = typecheck_expr(prog, scope, item);
                        int len = 1;
                        while (get_node(item)->next_sibling != 0) {
                            len++;
                            item = get_node(item)->next_sibling;
                            if (!is_type_eq(t, typecheck_expr(prog, scope, item))) {
                                fatal_error(get_node(item)->start_token, "Unmatched array item type");
                            }
                        }
                        t = make_array_type(len, t);
                    }
                    break;
            }
            break;
        case EXPR_CALL:
            {
                // is it a built-in op?
                AstNode *callee = get_node(n->expr.call.callee);
                if (callee->type == AST_EXPR && callee->expr.type == EXPR_IDENT) {
                    typecheck_expr(prog, scope, n->expr.call.callee);
                    Symbol *fn_sym = lookup_program_symbol(prog, callee->expr.ident);

                    if (fn_sym == NULL) {
                        fatal_error(callee->start_token, "call to undefined function '%.*s'",
                                (int)callee->expr.ident.len, callee->expr.ident.s);
                    }
                    AstNode *fn = get_node(fn_sym->obj);

                    if (fn->type != AST_FN) {
                        fatal_error(callee->start_token, "call to something that is not a function");
                    }
                    if (ast_node_sibling_size(fn->fn.first_arg) !=
                        ast_node_sibling_size(n->expr.call.first_arg)) {
                        fatal_error(callee->start_token, "function '%.*s' expected %d arguments but %d given",
                                (int)callee->expr.ident.len, callee->expr.ident.s,
                                ast_node_sibling_size(fn->fn.first_arg),
                                ast_node_sibling_size(n->expr.call.first_arg));
                    }
                    // check each argument
                    Vec *argdef = &get_type(fn_sym->type)->func.args;
                    NodeIdx arg = n->expr.call.first_arg;

                    for (int i=0; i<argdef->len; ++i, arg = get_node(arg)->next_sibling) {
                        TypeId expected_type = *(TypeId*)vec_get(argdef, i);
                        TypeId passed_type = typecheck_expr(prog, scope, arg);
                        if (!is_type_eq(passed_type, expected_type)) {
                            fatal_error(get_node(arg)->start_token, "error passing argument %d: type %.*s does not match expected type %.*s",
                                    i + 1,
                                    (int)get_type(passed_type)->name.len,
                                    get_type(passed_type)->name.s,
                                    (int)get_type(expected_type)->name.len,
                                    get_type(expected_type)->name.s);
                        }
                    }

                    assert(get_type(fn->fn.type)->type == TT_FUNC);
                    t = get_type(fn->fn.type)->func.ret;
                } else {
                    fatal_error(callee->start_token, "fn call by expression not implemented");
                }
            }
            break;
        case EXPR_IF_ELSE:
            {
                TypeId cond = typecheck_expr(prog, scope, n->expr.if_else.condition);

                if (cond != U8 && cond != U16) {
                    fatal_error(n->start_token, "expected U8 or U16 if condition, but found '%.*s'",
                            (int)get_type(cond)->name.len,
                            get_type(cond)->name.s);
                }

                TypeId on_true = typecheck_expr(prog, scope, n->expr.if_else.on_true);

                if (n->expr.if_else.on_false != 0) {
                    TypeId on_false = typecheck_expr(prog, scope, n->expr.if_else.on_false);

                    if (!is_type_eq(on_false, on_true)) {
                        fatal_error(n->start_token, "if-else expects both branches to evaluate to the same type. found %.*s and %.*s",
                                (int)get_type(on_true)->name.len,
                                get_type(on_true)->name.s,
                                (int)get_type(on_false)->name.len,
                                get_type(on_false)->name.s);
                    }
                }

                t = on_true;
            }
            break;
        case EXPR_WHILE_LOOP:
            {
                TypeId cond = typecheck_expr(prog, scope, n->expr.while_loop.condition);

                if (cond != U8 && cond != U16) {
                    fatal_error(n->start_token, "expected U8 or U16 while condition, but found '%.*s'",
                            (int)get_type(cond)->name.len,
                            get_type(cond)->name.s);
                }
                label_push(scope, (JumpLabel) {
                    .label = n->expr.while_loop.label,
                    .node = expr
                });
                typecheck_expr(prog, scope, n->expr.while_loop.body);
                label_pop(scope);

                t = VOID;
            }
            break;
        case EXPR_LOCAL_SCOPE:
            {
                /*printf("LOCAL SCOPE TYPE: %.*s %.*s\n",
                    (int)n->expr.local_scope.var_name.len,
                    n->expr.local_scope.var_name.s,
                    (int)get_type(n->expr.local_scope.var_type)->name.len,
                    get_type(n->expr.local_scope.var_type)->name.s);
                    */
                // infer type of local variable from assignment
                if (n->expr.local_scope.var_type == TYPE_UNKNOWN &&
                    n->expr.local_scope.value != 0) {
                    n->expr.local_scope.var_type = typecheck_expr(prog, scope, n->expr.local_scope.value);
                }

                scope_push(scope, (Variable) {
                    .name = n->expr.local_scope.var_name,
                    .type = n->expr.local_scope.var_type
                });
                t = typecheck_expr(prog, scope, n->expr.local_scope.scoped_expr);
                scope_pop(scope);
            }
            break;
        case EXPR_BUILTIN:
            t = typecheck_builtin(prog, scope, expr);
            break;
    }
    n->expr.eval_type = t;
    return t;
}

static void typecheck_fn(Program *prog, NodeIdx fn) {
    AstNode *fn_node = get_node(fn);

    // ignore extern / forward declarations
    if (fn_node->fn.body == 0) {
        return;
    }

    Scope scope = new_localscope();
    /* Add function arguments to scope */
    for (NodeIdx arg=fn_node->fn.first_arg; arg != 0; arg=get_node(arg)->next_sibling) {
        assert(get_node(arg)->type == AST_FN_ARG);
        
        scope_push(&scope, (Variable) {
            .name = get_node(arg)->fn_arg.name,
            .type = get_node(arg)->fn_arg.type
        });
    }

    TypeId returned = typecheck_expr(prog, &scope, fn_node->fn.body);
    TypeId expected = get_type(fn_node->fn.type)->func.ret;

    if (!is_type_eq(expected, returned)) {
        fatal_error(fn_node->start_token, "function %.*s returned %.*s but should return %.*s",
                (int)fn_node->fn.name.len,
                fn_node->fn.name.s,
                (int)get_type(returned)->name.len,
                get_type(returned)->name.s,
                (int)get_type(expected)->name.len,
                get_type(expected)->name.s);
    }

    free_localscope(&scope);
}

static void check_is_literal(Program *prog, NodeIdx node) {
    AstNode *n = get_node(node);
    assert(n->type == AST_EXPR);
    if (n->expr.type != EXPR_LITERAL) {
        fatal_error(n->start_token, "Expected literal");
    }
    switch (n->expr.literal.type) {
        case LIT_ARRAY:
            check_is_literal(prog, n->expr.literal.literal_array_first_val);
            break;
        case LIT_U8:
        case LIT_U16:
        case LIT_VOID:
        case LIT_STR:
            break;
    }
}

static void typecheck_global(Program *prog, NodeIdx node) {
    AstNode *n = get_node(node);
    assert(n->type == AST_DEF_VAR);

    if (n->var_def.is_const == false && n->var_def.value == 0) return;
    assert(n->var_def.value != 0);

    check_is_literal(prog, n->var_def.value);
    TypeId v = typecheck_expr(prog, NULL, n->var_def.value);

    if (n->var_def.type == TYPE_UNKNOWN) {
        n->var_def.type = v;
        lookup_program_symbol(prog, n->var_def.name)->type = v;
    }

    if (!is_type_eq(v, n->var_def.type)) {
        fatal_error(n->start_token, "invalid type assigned to const: expected '%.*s' but found '%.*s'",
                (int)get_type(n->var_def.type)->name.len,
                get_type(n->var_def.type)->name.s,
                (int)get_type(v)->name.len,
                get_type(v)->name.s);
    }
}

/** deduces the eval_type of AST_EXPR nodes, and checks for type errors. */
void typecheck_program(Program *prog) {
    for (NodeIdx node=get_node(prog->root)->module.first_child; node != 0; node=get_node(node)->next_sibling) {
        if (get_node(node)->type == AST_FN) {
            typecheck_fn(prog, node);
        }
        if (get_node(node)->type == AST_DEF_VAR) {
            typecheck_global(prog, node);
        }
    }
}

Program new_program() {
    Program prog = {
        .root = 0,
        .symbols = vec_init(sizeof(Symbol))
    };

    //collect_symbols(&prog);
    //typecheck_program(&prog);

    return prog;
}

void free_program(Program *prog) {
    vec_free(&prog->symbols);
}
