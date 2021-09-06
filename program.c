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
typedef Vec Scope;
static Vec new_localscope() { return vec_init(sizeof(Variable)); }
static void free_localscope(Vec *scope) { vec_free(scope); }
static void scope_push(Vec *scope, Variable var) { vec_push(scope, &var); }
static void scope_pop(Vec *scope) { vec_pop(scope, NULL); }
static Variable *scope_lookup(Vec *scope, Str name) {
    for (int i=0; i<scope->len; ++i) {
        Variable *var = vec_get(scope, i);
        if (Str_eq2(var->name, name)) return var;
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
    { BUILTIN_ASSIGN, TT_PTR, TT_PTR, -1 },
    { BUILTIN_ARRAY_INDEXING, TT_ARRAY, TT_PRIM_U8, -1 /* any */ },
    { BUILTIN_ARRAY_INDEXING, TT_ARRAY, TT_PRIM_U16, -1 /* any */ },
    { BUILTIN_ADD, TT_PTR, TT_PRIM_U16, -1 },

    { BUILTIN_UNARY_NEG, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_ADD, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_SUB, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_NEQ, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_EQ, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_GT, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_LT, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_MUL, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_BITXOR, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_BITAND, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_BITOR, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_ASSIGN, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_PLUSASSIGN, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { BUILTIN_UNARY_NEG, TT_PRIM_U8, TT_PRIM_VOID, U8 },
    { BUILTIN_UNARY_BITNOT, TT_PRIM_U8, TT_PRIM_VOID, U8 },
    { BUILTIN_UNARY_LOGICAL_NOT, TT_PRIM_U8, TT_PRIM_VOID, U8 },

    { BUILTIN_ADD, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_SUB, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_NEQ, TT_PRIM_U16, TT_PRIM_U16, U8 },
    { BUILTIN_EQ, TT_PRIM_U16, TT_PRIM_U16, U8 },
    { BUILTIN_GT, TT_PRIM_U16, TT_PRIM_U16, U8 },
    { BUILTIN_LT, TT_PRIM_U16, TT_PRIM_U16, U8 },
    { BUILTIN_MUL, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_BITXOR, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_BITAND, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_BITOR, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_ASSIGN, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_PLUSASSIGN, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { BUILTIN_UNARY_NEG, TT_PRIM_U16, TT_PRIM_VOID, U16 },
    { BUILTIN_UNARY_BITNOT, TT_PRIM_U16, TT_PRIM_VOID, U16 },
    { BUILTIN_UNARY_LOGICAL_NOT, TT_PRIM_U16, TT_PRIM_VOID, U8 },

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
                        if (tt1 == TT_PTR) return t1;
                        else assert(false);
                    case BUILTIN_ASSIGN:
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
    fatal_error(n->start_token, "invalid arguments to operator %s: '%.*s' and '%.*s'",
            builtin_name(op),
            (int)get_type(t1)->name.len,
            get_type(t1)->name.s,
            (int)get_type(t2)->name.len,
            get_type(t2)->name.s);
}


static TypeId typecheck_expr(Program *prog, Scope *scope, NodeIdx expr) {
    AstNode *n = get_node(expr);
    TypeId t = TYPE_UNKNOWN;
    switch (n->expr.type) {
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
                    (get_type(from_type)->type == TT_PTR && t == U16)
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
        case EXPR_LITERAL_U8:
            t = U8;
            break;
        case EXPR_LITERAL_U16:
            t = U16;
            break;
        case EXPR_LITERAL_VOID:
            t = VOID;
            break;
        case EXPR_CALL:
            {
                // is it a built-in op?
                AstNode *callee = get_node(n->expr.call.callee);
                if (callee->type == AST_EXPR && callee->expr.type == EXPR_IDENT) {
                    if (Str_eq(callee->expr.ident, "asm")) {
                        // emit literal asm
                        AstNode *arg = get_node(n->expr.call.first_arg);
                        if (arg->type != AST_EXPR || arg->expr.type != EXPR_LITERAL_STR) {
                            fatal_error(callee->start_token, "asm() expects string literal argument");
                        }
                        if (arg->next_sibling != 0) {
                            fatal_error(callee->start_token, "asm() takes only one argument");
                        }
                        // ok
                        t = VOID;
                    } else {
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
                    }
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
                typecheck_expr(prog, scope, n->expr.while_loop.body);

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
        case EXPR_LITERAL_STR:
            assert(false);
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

/** deduces the eval_type of AST_EXPR nodes, and checks for type errors. */
void typecheck_program(Program *prog) {
    for (NodeIdx node=get_node(prog->root)->module.first_child; node != 0; node=get_node(node)->next_sibling) {
        if (get_node(node)->type == AST_FN) {
            typecheck_fn(prog, node);
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
