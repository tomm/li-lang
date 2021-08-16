#include "output_lr35902.h"
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include "str.h"
#include "vec.h"
#include "tokenizer.h"
#include "types.h"

static void compile_error(const AstNode *at, const char *format, ...) __attribute__((format(printf, 2, 3)));
static void compile_error(const AstNode *at, const char *format, ...) {
	char buf[1024];
	va_list ap;
	va_start(ap, format);
	vsnprintf(buf, sizeof(buf), format, ap);
	va_end(ap);
    fprintf(stderr, "%d:%d: %s\n",
            at->start_token->line, at->start_token->col, buf);
    exit(-1);
}

static TypeId find_type(const AstNode *n, Str typename) {
    printf("Looking up type %.*s\n", (int)typename.len, typename.s);
    TypeId id = lookup_type(typename);
    if (id == -1) {
        compile_error(n, "Unknown type %.*s", (int)typename.len, typename.s);
    }
    return id;
}

static void emit_boilerplate(FILE *out) {
    fputs(
"SECTION \"Header\", ROM0[$100]\n"
"\t\tdi\n"
"\t\tjp __start\n"
"\t\t; Space for cartridge headers\n"
"\t\tREPT $150 - $104\n"
"\t\t\tdb 0\n"
"\t\tENDR\n"
"\n"
"SECTION \"Code\", ROM0[$150]\n"
"__start:\n"
"\t\tcall main\n"
"\t\tdb $dd ; debug dump of register set\n"
"\t\thalt\n"
    , out);
}

/*         ST_REG_EA ST_REG_VAL ST_REG_VAL_AUX
 *  U8            hl          a              b
 *  U16           hl         hl             de
 */
typedef struct Value {
    TypeId typeId;
    enum Storage {
        ST_REG_EA,
        ST_REG_VAL,
        ST_REG_VAL_AUX
    } storage;
} Value;

typedef int StackVarIdx;
typedef struct StackVar {
    TypeId type;
    Str ident;
    int offset; // from SP on function entry
} StackVar;

typedef struct StackFrame {
    int num_vars;
    int stack_offset;
} StackFrame;

Vec /*<StackVar>*/ _stack_vars;

/* base pointer (bp) is implicit. we only really have a stack pointer */
static StackVarIdx alloc_var() {
    StackVar v;
    memset(&v, 0, sizeof(StackVar));
    StackVarIdx idx = _stack_vars.len;
    vec_push(&_stack_vars, &v);
    return idx;
}

/* opcode output */
static void _(FILE *f, const char *format, ...) __attribute__((format(printf, 2, 3)));
static void _(FILE *f, const char *format, ...) {
	char buf[256];
	va_list ap;
	va_start(ap, format);
	vsnprintf(buf, sizeof(buf), format, ap);
	va_end(ap);
    fputs("\t\t", f);
    fputs(buf, f);
    fputs("\n", f);
}
static StackVar *get_stack_var(StackVarIdx idx) {
    return vec_get(&_stack_vars, idx);
}

static Value emit_value_to_register(FILE *out, Value v, bool to_aux_reg) {
    assert(v.typeId == U8 || v.typeId == U16);
    const enum Storage st = to_aux_reg ? ST_REG_VAL_AUX : ST_REG_VAL;

    if (v.typeId == U8) {
        switch (v.storage) {
            case ST_REG_EA:
                _(out, "ld %s, [hl]", to_aux_reg ? "b" : "a");
                return (Value) { .typeId = U8, .storage = st };
            case ST_REG_VAL:
                if (st != v.storage) {
                    _(out, "ld b, a");
                }
                return (Value) { .typeId = U8, .storage = st };
            case ST_REG_VAL_AUX:
                if (st != v.storage) {
                    _(out, "ld a, b");
                }
                return (Value) { .typeId = U8, .storage = st };
        }
    } else {
        switch (v.storage) {
            case ST_REG_EA:
                _(out, "ld a, [hl+]");
                _(out, "ld %s, [hl]", to_aux_reg ? "d" : "h");
                _(out, "ld %s, a", to_aux_reg ? "e" : "l");
                return (Value) { .typeId = U16, .storage = st };
            case ST_REG_VAL:
                if (st != v.storage) {
                    _(out, "ld d, h");
                    _(out, "ld e, l");
                }
                return (Value) { .typeId = U16, .storage = st };
            case ST_REG_VAL_AUX:
                if (st != v.storage) {
                    _(out, "ld hl, de");
                }
                return (Value) { .typeId = U16, .storage = st };
        }
    }
}

static void emit_push(FILE *out, Value v, StackFrame *frame) {
    switch (v.storage) {
        case ST_REG_VAL:
            switch (v.typeId) {
                case VOID: assert(false);
                case U8:
                    _(out, "push af");
                    frame->stack_offset += 2;
                    return;
                case U16:
                    _(out, "push hl");
                    frame->stack_offset += 2;
                    return;
                default: assert(false);
            }
        case ST_REG_EA:
            switch (v.typeId) {
                case VOID: assert(false);
                case U8:
                    _(out, "push hl");
                    frame->stack_offset += 2;
                    return;
                case U16:
                    _(out, "push hl");
                    frame->stack_offset += 2;
                    return;
                default: assert(false);
            }
    }
}

static Value emit_pop(FILE *out, Value v, StackFrame *frame, bool to_aux_reg) {
    const enum Storage st = to_aux_reg ? ST_REG_VAL_AUX : ST_REG_VAL;

    switch (v.storage) {
        case ST_REG_VAL_AUX:
            return v;
        case ST_REG_VAL:
            switch (v.typeId) {
                case VOID: assert(false);
                case U8:
                    _(out, "pop %s", to_aux_reg ? "bc" : "af");
                    frame->stack_offset -= 2;
                    return (Value) { .storage = st, .typeId = U8 };
                case U16:
                    _(out, "pop %s", to_aux_reg ? "de" : "hl");
                    frame->stack_offset -= 2;
                    return (Value) { .storage = st, .typeId = U16 };
                default: assert(false);
            }
        case ST_REG_EA:
            switch (v.typeId) {
                case VOID: assert(false);
                case U8:
                    _(out, "pop hl");
                    frame->stack_offset -= 2;
                    return (Value) { .storage = ST_REG_EA, .typeId = U8 };
                case U16:
                    _(out, "pop hl");
                    frame->stack_offset -= 2;
                    return (Value) { .storage = ST_REG_EA, .typeId = U16 };
                default: assert(false);
            }
    }
}
static Value emit_expression(FILE *out, NodeIdx expr, StackFrame frame);

static Value emit_builtin_u8(FILE *out, StackFrame *frame, enum BuiltinOp op, Value v1, Value v2) {
    // v1 on stack, v2 as tmp (active Value)
    v2 = emit_value_to_register(out, v2, true);   // v2 in `b`
    v1 = emit_pop(out, v1, frame, false);
    v1 = emit_value_to_register(out, v1, false);  // v1 in `a`

    // assumes binary op
    switch (op) {
        case BUILTIN_ADD:
            _(out, "add a, b");
            break;
        case BUILTIN_SUB:
            _(out, "sub a, b");
            break;
        case BUILTIN_BITOR:
            _(out, "or a, b");
            break;
        case BUILTIN_BITAND:
            _(out, "and a, b");
            break;
        case BUILTIN_BITXOR:
            _(out, "xor a, b");
            break;
        default:
            assert(false);
            break;
    }
    return (Value) { .typeId = U8, .storage = ST_REG_VAL };
}

static Value emit_builtin_u16(FILE *out, StackFrame *frame, enum BuiltinOp op, Value v1, Value v2) {
    // v1 on stack, v2 as tmp (active Value)
    v2 = emit_value_to_register(out, v2, true);   // v2 in `de`
    v1 = emit_pop(out, v1, frame, false);
    v1 = emit_value_to_register(out, v1, false);  // v1 in `hl`

    switch (op) {
        case BUILTIN_ADD:
            _(out, "add hl, de");
            break;
        case BUILTIN_SUB:
            _(out, "ld a, l");
            _(out, "sub a, e");
            _(out, "ld l, a");

            _(out, "ld a, h");
            _(out, "sbc a, d");
            _(out, "ld h, a");
            break;
        case BUILTIN_BITOR:
            _(out, "ld a, h");
            _(out, "or a, d");
            _(out, "ld h, a");

            _(out, "ld a, l");
            _(out, "or a, e");
            _(out, "ld l, a");
            break;
        case BUILTIN_BITAND:
            _(out, "ld a, h");
            _(out, "and a, d");
            _(out, "ld h, a");

            _(out, "ld a, l");
            _(out, "and a, e");
            _(out, "ld l, a");
            break;
        case BUILTIN_BITXOR:
            _(out, "ld a, h");
            _(out, "xor a, d");
            _(out, "ld h, a");

            _(out, "ld a, l");
            _(out, "xor a, e");
            _(out, "ld l, a");
            break;
        default:
            assert(false);
            break;
    }
    return (Value) { .typeId = U16, .storage = ST_REG_VAL };
}

static Value emit_builtin(FILE *out, NodeIdx call, StackFrame frame) {
    AstNode *n = get_node(call);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_BUILTIN);

    AstNode *arg1 = get_node(n->expr.builtin.first_arg);
    assert(arg1->next_sibling != 0);

    Value v1 = emit_expression(out, n->expr.builtin.first_arg, frame);
    emit_push(out, v1, &frame);
    Value v2 = emit_expression(out, arg1->next_sibling, frame);

    if (!is_type_eq(v2.typeId, v1.typeId)) {
        compile_error(n, "builtin operator expects same types. found %.*s and %.*s",
                get_type(v2.typeId)->name.len,
                get_type(v2.typeId)->name.s,
                get_type(v1.typeId)->name.len,
                get_type(v1.typeId)->name.s);
    }


    switch (v2.typeId) {
        case U8: return emit_builtin_u8(out, &frame, n->expr.builtin.op, v1, v2);
        case U16: return emit_builtin_u16(out, &frame, n->expr.builtin.op, v1, v2);
        default: assert(false);
    }
}

static Value emit_cast(FILE *out, NodeIdx cast, StackFrame frame) {
    AstNode *n = get_node(cast);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_CAST);

    TypeId to_type = find_type(n, n->expr.cast.to_type);
    Value v1 = emit_expression(out, n->expr.cast.arg, frame);

    if (v1.typeId == to_type) {
        // no-op
    } else if (v1.typeId == U8 && to_type == U16) {
        v1 = emit_value_to_register(out, v1, false);  // v1 in `a`
        _(out, "ld h, 0");
        _(out, "ld l, a");
    } else if (v1.typeId == U16 && to_type == U8) {
        v1 = emit_value_to_register(out, v1, false);  // v1 in `hl`
        _(out, "ld a, l");
    } else {
        compile_error(n, "Invalid type cast (from %.*s to %.*s)",
                (int)get_type(v1.typeId)->name.len,
                get_type(v1.typeId)->name.s,
                (int)get_type(to_type)->name.len,
                get_type(to_type)->name.s);
    }

    return (Value) { .typeId = to_type, .storage = ST_REG_VAL };
}

static void emit_call_push_args(FILE *out, int arg_num, NodeIdx first_arg_type, NodeIdx arg_list_head, StackFrame *frame) {
    // push last to first
    if (arg_list_head == 0) {
        return;
    }

    // expected arg type from definition
    AstNode *arg_type = get_node(first_arg_type);
    assert(arg_type->type == AST_FN_ARG);

    AstNode *n = get_node(arg_list_head);
    if (n->next_sibling != 0) {
        emit_call_push_args(out, arg_num+1, arg_type->next_sibling, n->next_sibling, frame);
    }

    assert(n->type == AST_EXPR);
    Value v = emit_expression(out, arg_list_head, *frame);
    emit_push(out, v, frame);

    const Type *expected = get_type(find_type(n, arg_type->fn_arg.type));

    if (!is_type_eq(v.typeId, find_type(n, arg_type->fn_arg.type))) {
        compile_error(n, "error passing argument %d: type %.*s does not match expected type %.*s",
                arg_num + 1,
                (int)get_type(v.typeId)->name.len,
                get_type(v.typeId)->name.s,
                (int)expected->name.len,
                expected->name.s);
    }
}

// includes self
static int ast_node_sibling_size(NodeIdx n) {
    if (n == 0) return 0;
    else return 1 + ast_node_sibling_size(get_node(n)->next_sibling);
}

static const AstNode *lookup_fn(Str name) {
    // XXX assumes root of AST is index 0
    AstNode *mod = get_node(0);

    for (int i=mod->module.first_child; i!=0; i=get_node(i)->next_sibling) {
        AstNode *child = get_node(i);
        if (child->type != AST_FN) continue;
        if (Str_eq2(child->fn.name, name)) return child;
    }
    return NULL;
}

static Value emit_call(FILE *out, NodeIdx call, StackFrame frame) {
    AstNode *n = get_node(call);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_CALL);

    // is it a built-in op?
    AstNode *callee = get_node(n->expr.call.callee);
    if (callee->type == AST_EXPR && callee->expr.type == EXPR_IDENT) {
        const AstNode *fn = lookup_fn(callee->expr.ident);

        if (fn == NULL) {
            compile_error(callee, "call to undefined function '%.*s'",
                    (int)callee->expr.ident.len, callee->expr.ident.s);
        }
        if (ast_node_sibling_size(fn->fn.first_arg) !=
            ast_node_sibling_size(n->expr.call.first_arg)) {
            compile_error(callee, "function '%.*s' expected %d arguments but %d given",
                    (int)callee->expr.ident.len, callee->expr.ident.s,
                    ast_node_sibling_size(fn->fn.first_arg),
                    ast_node_sibling_size(n->expr.call.first_arg));
        }

        const int old_stack = frame.stack_offset;
        emit_call_push_args(out, 0, fn->fn.first_arg, n->expr.call.first_arg, &frame);
        // XXX does not check function exists, or check argument types!
        _(out, "call %.*s", (int)callee->expr.ident.len, callee->expr.ident.s);
        const int stack_correction = frame.stack_offset - old_stack;
        if (stack_correction) {
            _(out, "add sp, %d", stack_correction);
        }
        frame.stack_offset += stack_correction;
        return (Value) { .typeId = find_type(fn, fn->fn.ret), .storage = ST_REG_VAL };
    } else {
        compile_error(callee, "fn call by expression not implemented");
    }
}

const StackVar *lookup_stack_var(Str ident, StackFrame frame)
{
    for (int i=0; i<frame.num_vars; ++i) {
        const StackVar *var = vec_get(&_stack_vars, i);
        if (Str_eq2(ident, var->ident)) {
            return var;
        }
    }
    return NULL;
}

static Value emit_identifier(FILE *out, NodeIdx expr, StackFrame frame) {
    AstNode *n = get_node(expr);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_IDENT);

    const StackVar *var = lookup_stack_var(n->expr.ident, frame);
    if (var == NULL) {
        compile_error(n, "Variable '%.*s' is not defined", (int)n->expr.ident.len, n->expr.ident.s);
    }

    // bytes are pushed onto the stack as 2 bytes (push af), meaning there is an additional
    // offset of 1 to get to the byte corresponding to 'a', rather than the 'f' garbage...
    const int type_weirdness_offset = var->type == U8 ? 1 : 0;

    _(out, "ld hl, sp%+d", var->offset + frame.stack_offset + type_weirdness_offset);
    return (Value) { .typeId = var->type, .storage = ST_REG_EA };
}

static Value emit_expression(FILE *out, NodeIdx expr, StackFrame frame) {
    Value v = (Value) { .typeId = VOID, .storage = ST_REG_VAL };
    AstNode *n = get_node(expr);
    assert(n->type == AST_EXPR);

    switch (n->expr.type) {
        case EXPR_BUILTIN:
            return emit_builtin(out, expr, frame);
        case EXPR_CALL:
            return emit_call(out, expr, frame);
        case EXPR_LIST:
            for (NodeIdx e=n->expr.list.first_child; e != 0; e=get_node(e)->next_sibling) {
                v = emit_expression(out, e, frame);
            }
            break;
        case EXPR_IDENT:
            return emit_identifier(out, expr, frame);
        case EXPR_LITERAL_U8:
            _(out, "ld a, $%x", n->expr.literal_int);
            v = (Value) { .typeId = U8, .storage = ST_REG_VAL };
            break;
        case EXPR_LITERAL_U16:
            _(out, "ld hl, $%x", n->expr.literal_int);
            v = (Value) { .typeId = U16, .storage = ST_REG_VAL };
            break;
        case EXPR_CAST:
            return emit_cast(out, expr, frame);
    }
    return v;
}

static Value emit_fn(FILE *out, NodeIdx fn) {
    AstNode *fn_node = get_node(fn);
    assert(fn_node->type == AST_FN);

    Str_puts(fn_node->fn.name, out);
    fputs(":\n", out);

    vec_zero(&_stack_vars);

    // build stackframe of fn arguments
    int bp_offset = 2; // return address at 0(sp), 1(sp)
    for (NodeIdx arg=fn_node->fn.first_arg; arg != 0; arg=get_node(arg)->next_sibling) {
        assert(get_node(arg)->type == AST_FN_ARG);
        
        TypeId argtype = find_type(get_node(arg), get_node(arg)->fn_arg.type);

        StackVarIdx v = alloc_var();
        *get_stack_var(v) = (StackVar) {
            .type = argtype,
            .ident = get_node(arg)->fn_arg.name,
            .offset = bp_offset
        };
        bp_offset += get_type(argtype)->stack_size;
    }

    for (int i=0; i<_stack_vars.len; ++i) {
        StackVar *v = get_stack_var(i);
        fprintf(stderr, "var %.*s: %d(bp)\n", (int)v->ident.len, v->ident.s, v->offset);
    }

    StackFrame frame = { .num_vars = _stack_vars.len, .stack_offset = 0 };
    
    Value ret_val = emit_expression(out, fn_node->fn.body, frame);
    emit_value_to_register(out, ret_val, false);
    _(out, "ret");

    TypeId expected_ret = find_type(fn_node, fn_node->fn.ret);
    if (!is_type_eq(expected_ret, ret_val.typeId)) {
        compile_error(fn_node, "function %.*s returned %.*s but should return %.*s",
                (int)fn_node->fn.name.len,
                fn_node->fn.name.s,
                (int)get_type(ret_val.typeId)->name.len,
                get_type(ret_val.typeId)->name.s,
                (int)get_type(expected_ret)->name.len,
                get_type(expected_ret)->name.s);
    }

    return ret_val;
}

static void init() {
    _stack_vars = vec_init(sizeof(StackVar));
}

void output_lr35902(NodeIdx root) {
    init();
    init_types();

    FILE *out = fopen("out.asm", "w");

    AstNode *root_node = get_node(root);
    assert(root_node->type == AST_MODULE);

    emit_boilerplate(out);

    for (NodeIdx fn=root_node->module.first_child; fn != 0; fn=get_node(fn)->next_sibling) {
        emit_fn(out, fn);
    }

    fclose(out);
}
