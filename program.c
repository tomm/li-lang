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

static TypeId typecheck_builtin(Program *prog, Scope *scope, NodeIdx expr) {
    AstNode *n = get_node(expr);
    TypeId t = TYPE_UNKNOWN;
    enum BuiltinOp op = n->expr.builtin.op;

    switch (op) {
        case BUILTIN_ARRAY_INDEXING:
            {
                if (ast_node_sibling_size(n->expr.builtin.first_arg) != 2) {
                    fatal_error(n->start_token, "Malformed array indexing");
                }
                TypeId arr = typecheck_expr(prog, scope, n->expr.builtin.first_arg);
                TypeId idx = typecheck_expr(prog, scope, get_node(n->expr.builtin.first_arg)->next_sibling);

                if (get_type(arr)->type != TT_ARRAY) {
                    fatal_error(n->start_token, "Array index on non-array type '%.*s'",
                            (int)get_type(arr)->name.len,
                            get_type(arr)->name.s);
                }
                if (idx != U16 && idx != U8) {
                    fatal_error(n->start_token, "Array index must be u8 or u16 type");
                }

                t = get_type(arr)->array.contained;
            }
            break;
        default:
            // Expect argument types to match
            assert (ast_node_sibling_size(n->expr.builtin.first_arg) > 0);
            NodeIdx arg = n->expr.builtin.first_arg;
            t = typecheck_expr(prog, scope, arg);
            while (get_node(arg)->next_sibling != 0) {
                arg = get_node(arg)->next_sibling;
                TypeId t2 = typecheck_expr(prog, scope, arg);
                if (!is_type_eq(t2, t)) {
                    fatal_error(get_node(arg)->start_token, "operator expects matching argument types. found '%.*s' and '%.*s'",
                            (int)get_type(t)->name.len,
                            get_type(t)->name.s,
                            (int)get_type(t2)->name.len,
                            get_type(t2)->name.s);
                }
            }
            // actually boolean, but using u8 for now
            if (op == BUILTIN_EQ || op == BUILTIN_NEQ) {
                t = U8;
            }
            break;
    }

    return t;
}


static TypeId typecheck_expr(Program *prog, Scope *scope, NodeIdx expr) {
    AstNode *n = get_node(expr);
    TypeId t = TYPE_UNKNOWN;
    switch (n->expr.type) {
        case EXPR_ASM:
            t = VOID;
            break;
        case EXPR_CAST:
            typecheck_expr(prog, scope, n->expr.cast.arg);
            t = n->expr.cast.to_type;
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
