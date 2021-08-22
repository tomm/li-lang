#include "output_lr35902.h"
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include "str.h"
#include "vec.h"
#include "tokenizer.h"
#include "types.h"

static FILE *output;
static int _local_label_seq = 0;

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
    //printf("Looking up type %.*s\n", (int)typename.len, typename.s);
    TypeId id = lookup_type(typename);
    if (id == -1) {
        compile_error(n, "Unknown type %.*s", (int)typename.len, typename.s);
    }
    return id;
}

static void __(const char *format, ...) __attribute__((format(printf, 1, 2)));
static void __(const char *format, ...) {
	char buf[256];
	va_list ap;
	va_start(ap, format);
	vsnprintf(buf, sizeof(buf), format, ap);
	va_end(ap);
    fputs(buf, output);
    fputs("\n", output);
}

/* opcode output */
static void _i(const char *format, ...) __attribute__((format(printf, 1, 2)));
static void _i(const char *format, ...) {
	char buf[256];
	va_list ap;
	va_start(ap, format);
	vsnprintf(buf, sizeof(buf), format, ap);
	va_end(ap);
    fputs("        ", output);
    __("%s", buf);
}

static void _label(int label_num) {
    fputs("    ", output);
    __(".l%d:", label_num);
}

static void emit_boilerplate() {
    __("SECTION \"Header\", ROM0[$100]");
    __("        nop");
    __("        jp __start");
    __("        ; Space for cartridge headers");
    __("        REPT $150 - $104");
    __("        db 0");
    __("        ENDR");

    __("SECTION \"Code\", ROM0[$150]");
    __("__start:");
    __("        di");
    __("        ld sp, $e000 ; top of WRAM");
    __("        call main");
    __("    .loop");
    __("        halt");
    __("        nop");
    __("        jr .loop");

    __("; 248 cycles");
    __("__mulu8:");
    __("    ; a = a*b");
    __("        ld c, a");
    __("        xor a, a");
    __("    REPT 8");
    __("        srl b");
    __("        jr nc, .no\\@");
    __("        add a, c");
    __("    .no\\@");
    __("        sla c");
    __("    ENDR");
    __("        ret");

    __("; 700 cycles");
    __("__mulu16:");
    __("    ; hl = hl*de");
    __("        ld b, h");
    __("        ld c, l");
    __("        xor a");
    __("        ld h, a");
    __("        ld l, a");
    __("    REPT 16");
    __("        srl b");
    __("        rr c");
    __("        jr nc, .no\\@");
    __("        add hl, de");
    __("    .no\\@");
    __("        sla e");
    __("        rl d");
    __("    ENDR");
    __("        ret");
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

typedef struct RamVariable {
    Str symbol_name;
    int size_bytes;
} RamVariable;

Vec /*<RamVariable>*/ _ram_vars;




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

static StackVar *get_stack_var(StackVarIdx idx) {
    return vec_get(&_stack_vars, idx);
}

static Value emit_value_to_register(Value v, bool to_aux_reg) {
    const enum Storage st = to_aux_reg ? ST_REG_VAL_AUX : ST_REG_VAL;

    if (v.typeId == VOID) {
        return (Value) { .typeId = VOID, .storage = ST_REG_VAL };
    }

    else if (v.typeId == U8) {
        switch (v.storage) {
            case ST_REG_EA:
                _i("ld %s, [hl]", to_aux_reg ? "b" : "a");
                return (Value) { .typeId = U8, .storage = st };
            case ST_REG_VAL:
                if (st != v.storage) {
                    _i("ld b, a");
                }
                return (Value) { .typeId = U8, .storage = st };
            case ST_REG_VAL_AUX:
                if (st != v.storage) {
                    _i("ld a, b");
                }
                return (Value) { .typeId = U8, .storage = st };
        }
    }
    
    else if (v.typeId == U16) {
        switch (v.storage) {
            case ST_REG_EA:
                _i("ld a, [hl+]");
                _i("ld %s, [hl]", to_aux_reg ? "d" : "h");
                _i("ld %s, a", to_aux_reg ? "e" : "l");
                return (Value) { .typeId = U16, .storage = st };
            case ST_REG_VAL:
                if (st != v.storage) {
                    _i("ld d, h");
                    _i("ld e, l");
                }
                return (Value) { .typeId = U16, .storage = st };
            case ST_REG_VAL_AUX:
                if (st != v.storage) {
                    _i("ld hl, de");
                }
                return (Value) { .typeId = U16, .storage = st };
        }
    }

    assert(false);
}

static void emit_push(AstNode *n, Value v, StackFrame *frame) {
    if (v.typeId == VOID) {
        compile_error(n, "can not use a void value");
        return;
    }
    switch (v.storage) {
        case ST_REG_VAL:
            switch (v.typeId) {
                case U8:
                    _i("push af");
                    frame->stack_offset += 2;
                    return;
                case U16:
                    _i("push hl");
                    frame->stack_offset += 2;
                    return;
                default: assert(false);
            }
        case ST_REG_EA:
            switch (v.typeId) {
                case U8:
                    _i("push hl");
                    frame->stack_offset += 2;
                    return;
                case U16:
                    _i("push hl");
                    frame->stack_offset += 2;
                    return;
                default: assert(false);
            }
    }
}

static Value emit_pop(Value v, StackFrame *frame, bool to_aux_reg) {
    const enum Storage st = to_aux_reg ? ST_REG_VAL_AUX : ST_REG_VAL;

    switch (v.storage) {
        case ST_REG_VAL_AUX:
            return v;
        case ST_REG_VAL:
            switch (v.typeId) {
                case VOID: assert(false);
                case U8:
                    _i("pop %s", to_aux_reg ? "bc" : "af");
                    frame->stack_offset -= 2;
                    return (Value) { .storage = st, .typeId = U8 };
                case U16:
                    _i("pop %s", to_aux_reg ? "de" : "hl");
                    frame->stack_offset -= 2;
                    return (Value) { .storage = st, .typeId = U16 };
                default: assert(false);
            }
        case ST_REG_EA:
            switch (v.typeId) {
                case VOID: assert(false);
                case U8:
                    _i("pop hl");
                    frame->stack_offset -= 2;
                    return (Value) { .storage = ST_REG_EA, .typeId = U8 };
                case U16:
                    _i("pop hl");
                    frame->stack_offset -= 2;
                    return (Value) { .storage = ST_REG_EA, .typeId = U16 };
                default: assert(false);
            }
    }
}
static Value emit_expression(NodeIdx expr, StackFrame frame);

static Value emit_builtin_u8(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, Value v1, Value v2) {
    // v1 on stack, v2 as tmp (active Value)
    v2 = emit_value_to_register(v2, true);   // v2 in `b`
    v1 = emit_pop(v1, frame, false);

    if (op == BUILTIN_ASSIGN) {
        if (v1.storage != ST_REG_EA) {
            compile_error(get_node(expr), "Can not assign to temporary");
        }
        _i("ld [hl], b");
        return (Value) { .typeId = U8, .storage = ST_REG_EA };
    }

    v1 = emit_value_to_register(v1, false);  // v1 in `a`

    // assumes binary op
    switch (op) {
        case BUILTIN_ADD:
            _i("add a, b");
            break;
        case BUILTIN_SUB:
            _i("sub a, b");
            break;
        case BUILTIN_BITOR:
            _i("or a, b");
            break;
        case BUILTIN_BITAND:
            _i("and a, b");
            break;
        case BUILTIN_BITXOR:
            _i("xor a, b");
            break;
        case BUILTIN_MUL:
            _i("call __mulu8");
            break;
        case BUILTIN_EQ:
            {
                const int l = _local_label_seq++;
                _i("cp a, b");
                _i("ld a, 0"); // clear without affecting flags
                _i("jr nz, .l%d", l);
                _i("dec a");
                _label(l);
            }
            break;
        case BUILTIN_NEQ:
            {
                _i("sub a, b");
            }
            break;
        default:
            assert(false);
            break;
    }
    return (Value) { .typeId = U8, .storage = ST_REG_VAL };
}

static Value emit_builtin_u16(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, Value v1, Value v2) {
    // v1 on stack, v2 as tmp (active Value)
    v2 = emit_value_to_register(v2, true);   // v2 in `de`
    v1 = emit_pop(v1, frame, false);

    if (op == BUILTIN_ASSIGN) {
        if (v1.storage != ST_REG_EA) {
            compile_error(get_node(expr), "Can not assign to temporary");
        }
        _i("ld a, e");
        _i("ld [hl+], a");
        _i("ld a, d");
        _i("ld [hl-], a");
        return (Value) { .typeId = U16, .storage = ST_REG_EA };
    }

    v1 = emit_value_to_register(v1, false);  // v1 in `hl`

    switch (op) {
        case BUILTIN_ADD:
            _i("add hl, de");
            break;
        case BUILTIN_SUB:
            _i("ld a, l");
            _i("sub a, e");
            _i("ld l, a");

            _i("ld a, h");
            _i("sbc a, d");
            _i("ld h, a");
            break;
        case BUILTIN_BITOR:
            _i("ld a, h");
            _i("or a, d");
            _i("ld h, a");

            _i("ld a, l");
            _i("or a, e");
            _i("ld l, a");
            break;
        case BUILTIN_BITAND:
            _i("ld a, h");
            _i("and a, d");
            _i("ld h, a");

            _i("ld a, l");
            _i("and a, e");
            _i("ld l, a");
            break;
        case BUILTIN_BITXOR:
            _i("ld a, h");
            _i("xor a, d");
            _i("ld h, a");

            _i("ld a, l");
            _i("xor a, e");
            _i("ld l, a");
            break;
        case BUILTIN_MUL:
            _i("call __mulu16");
            break;
        case BUILTIN_EQ:
            {
                const int l = _local_label_seq++;
                _i("ld a, l");
                _i("sub a, e");
                _i("ld l, a");

                _i("ld a, h");
                _i("sbc a, d");
                _i("or a, l");

                _i("ld a, 0"); // not affecting flags

                _i("jr nz, .l%d", l);
                _i("dec a");
                _label(l);
            }
            return (Value) { .typeId = U8, .storage = ST_REG_VAL };
        case BUILTIN_NEQ:
            {
                _i("ld a, l");
                _i("sub a, e");
                _i("ld l, a");

                _i("ld a, h");
                _i("sbc a, d");
                _i("or a, l");
            }
            return (Value) { .typeId = U8, .storage = ST_REG_VAL };
        default:
            assert(false);
            break;
    }
    return (Value) { .typeId = U16, .storage = ST_REG_VAL };
}

static Value emit_builtin(NodeIdx call, StackFrame frame) {
    AstNode *n = get_node(call);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_BUILTIN);

    AstNode *arg1 = get_node(n->expr.builtin.first_arg);
    assert(arg1->next_sibling != 0);

    Value v1 = emit_expression(n->expr.builtin.first_arg, frame);
    emit_push(arg1, v1, &frame);
    Value v2 = emit_expression(arg1->next_sibling, frame);

    if (!is_type_eq(v2.typeId, v1.typeId)) {
        compile_error(n, "builtin operator expects same types. found %.*s and %.*s",
                (int)get_type(v2.typeId)->name.len,
                get_type(v2.typeId)->name.s,
                (int)get_type(v1.typeId)->name.len,
                get_type(v1.typeId)->name.s);
    }


    switch (v2.typeId) {
        case U8: return emit_builtin_u8(call, &frame, n->expr.builtin.op, v1, v2);
        case U16: return emit_builtin_u16(call, &frame, n->expr.builtin.op, v1, v2);
        default: assert(false);
    }
}

static Value emit_cast(NodeIdx cast, StackFrame frame) {
    AstNode *n = get_node(cast);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_CAST);

    TypeId to_type = find_type(n, n->expr.cast.to_type);
    Value v1 = emit_expression(n->expr.cast.arg, frame);

    if (v1.typeId == to_type) {
        // no-op
    } else if (v1.typeId == U8 && to_type == U16) {
        v1 = emit_value_to_register(v1, false);  // v1 in `a`
        _i("ld h, 0");
        _i("ld l, a");
    } else if (v1.typeId == U16 && to_type == U8) {
        v1 = emit_value_to_register(v1, false);  // v1 in `hl`
        _i("ld a, l");
    } else {
        compile_error(n, "Invalid type cast (from %.*s to %.*s)",
                (int)get_type(v1.typeId)->name.len,
                get_type(v1.typeId)->name.s,
                (int)get_type(to_type)->name.len,
                get_type(to_type)->name.s);
    }

    return (Value) { .typeId = to_type, .storage = ST_REG_VAL };
}

static TypeId lookup_typeid_from_ast_typename_node(NodeIdx typename_) {
    AstNode *n = get_node(typename_);
    assert(n->type == AST_TYPENAME);

    return find_type(n, n->typename_.name);
}

static void emit_call_push_args(int arg_num, NodeIdx first_arg_type, NodeIdx arg_list_head, StackFrame *frame) {
    // push last to first
    if (arg_list_head == 0) {
        return;
    }

    // expected arg type from definition
    AstNode *arg_type = get_node(first_arg_type);
    assert(arg_type->type == AST_FN_ARG);

    AstNode *n = get_node(arg_list_head);
    if (n->next_sibling != 0) {
        emit_call_push_args(arg_num+1, arg_type->next_sibling, n->next_sibling, frame);
    }

    assert(n->type == AST_EXPR);
    Value v = emit_expression(arg_list_head, *frame);
    emit_push(n, v, frame);

    TypeId expected_typeid = lookup_typeid_from_ast_typename_node(arg_type->fn_arg.typename_);
    const Type *expected_type = get_type(expected_typeid);

    if (!is_type_eq(v.typeId, expected_typeid)) {
        compile_error(n, "error passing argument %d: type %.*s does not match expected type %.*s",
                arg_num + 1,
                (int)get_type(v.typeId)->name.len,
                get_type(v.typeId)->name.s,
                (int)expected_type->name.len,
                expected_type->name.s);
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

static Value emit_call(NodeIdx call, StackFrame frame) {
    AstNode *n = get_node(call);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_CALL);

    // is it a built-in op?
    AstNode *callee = get_node(n->expr.call.callee);
    if (callee->type == AST_EXPR && callee->expr.type == EXPR_IDENT) {
        if (Str_eq(callee->expr.ident, "asm")) {
            // emit literal asm
            AstNode *arg = get_node(n->expr.call.first_arg);
            if (arg->type != AST_EXPR || arg->expr.type != EXPR_LITERAL_STR) {
                compile_error(callee, "asm() expects string literal argument");
            }
            if (arg->next_sibling != 0) {
                compile_error(callee, "asm() takes only one argument");
            }
            fprintf(output, "\n%.*s\n", (int)arg->expr.literal_str.len, arg->expr.literal_str.s);
            return (Value) { .typeId = VOID, .storage = ST_REG_VAL };
        }

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
        emit_call_push_args(0, fn->fn.first_arg, n->expr.call.first_arg, &frame);
        // XXX does not check function exists, or check argument types!
        _i("call %.*s", (int)callee->expr.ident.len, callee->expr.ident.s);
        const int stack_correction = frame.stack_offset - old_stack;
        if (stack_correction) {
            _i("add sp, %d", stack_correction);
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

const AstNode *lookup_global_var(Str name)
{
    // XXX assumes root of AST is index 0
    AstNode *mod = get_node(0);

    for (int i=mod->module.first_child; i!=0; i=get_node(i)->next_sibling) {
        AstNode *child = get_node(i);
        if (child->type != AST_DEF_VAR) continue;
        if (Str_eq2(child->var_def.name, name)) return child;
    }
    return NULL;
}

static Value emit_if_else(NodeIdx expr, StackFrame frame) {
    AstNode *n = get_node(expr);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_IF_ELSE);

    Value condition = emit_expression(n->expr.if_else.condition, frame);

    const int else_label = _local_label_seq++;
    const int end_label = _local_label_seq++;

    if (condition.typeId == U8) {
        condition = emit_value_to_register(condition, false);  // to 'a' register
        _i("and a, a");
        _i("jp z, .l%d", else_label);
    }
    else if (condition.typeId == U16) {
        condition = emit_value_to_register(condition, false);  // to 'hl' register
        _i("ld a, l");
        _i("or a, h");
        _i("jp z, .l%d", else_label);
    } else {
        compile_error(n, "Invalid condition type in if expression. Expected u8 or u16, but found %.*s",
                (int)get_type(condition.typeId)->name.len,
                get_type(condition.typeId)->name.s);
        return (Value) { .typeId = VOID, .storage = ST_REG_VAL };
    }

    Value on_true = emit_expression(n->expr.if_else.on_true, frame);

    if (n->expr.if_else.on_false != 0) {
        _i("jp .l%d", end_label);
        _label(else_label);

        Value on_false = emit_expression(n->expr.if_else.on_false, frame);

        if (!is_type_eq(on_false.typeId, on_true.typeId)) {
            compile_error(n, "if-else expects both branches to evaluate to the same type. found %.*s and %.*s",
                    (int)get_type(on_true.typeId)->name.len,
                    get_type(on_true.typeId)->name.s,
                    (int)get_type(on_false.typeId)->name.len,
                    get_type(on_false.typeId)->name.s);
        }

        _label(end_label);
        // the storage class must be the same
        if (on_false.storage != on_true.storage) {
            compile_error(n, "storage class of if branches differs. THIS CODE IS UNFINISHED. giving up");
        }
    } else {
        _label(else_label);
    }
    return on_true;
}

static Value emit_identifier(NodeIdx expr, StackFrame frame) {
    AstNode *n = get_node(expr);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_IDENT);

    const StackVar *var = lookup_stack_var(n->expr.ident, frame);

    if (var) {
        // bytes are pushed onto the stack as 2 bytes (push af), meaning there is an additional
        // offset of 1 to get to the byte corresponding to 'a', rather than the 'f' garbage...
        const int type_weirdness_offset = var->type == U8 ? 1 : 0;

        _i("ld hl, sp%+d", var->offset + frame.stack_offset + type_weirdness_offset);
        return (Value) { .typeId = var->type, .storage = ST_REG_EA };
    }

    const AstNode *global = lookup_global_var(n->expr.ident);

    if (global) {
        assert(global->type == AST_DEF_VAR);
        _i("ld hl, %.*s", (int)n->expr.ident.len, n->expr.ident.s);
        return (Value) { .typeId = lookup_typeid_from_ast_typename_node(global->var_def.typename_), .storage = ST_REG_EA };
    }

    compile_error(n, "Variable '%.*s' is not defined", (int)n->expr.ident.len, n->expr.ident.s);
}

static Value emit_expression(NodeIdx expr, StackFrame frame) {
    Value v = (Value) { .typeId = VOID, .storage = ST_REG_VAL };
    AstNode *n = get_node(expr);
    assert(n->type == AST_EXPR);

    switch (n->expr.type) {
        case EXPR_BUILTIN:
            return emit_builtin(expr, frame);
        case EXPR_CALL:
            return emit_call(expr, frame);
        case EXPR_LIST:
            for (NodeIdx e=n->expr.list.first_child; e != 0; e=get_node(e)->next_sibling) {
                v = emit_expression(e, frame);
            }
            break;
        case EXPR_IDENT:
            return emit_identifier(expr, frame);
        case EXPR_LITERAL_U8:
            _i("ld a, $%x", n->expr.literal_int);
            v = (Value) { .typeId = U8, .storage = ST_REG_VAL };
            break;
        case EXPR_LITERAL_U16:
            _i("ld hl, $%x", n->expr.literal_int);
            v = (Value) { .typeId = U16, .storage = ST_REG_VAL };
            break;
        case EXPR_LITERAL_VOID:
            // the semicolon at the end of a list of expressions :)
            v = (Value) { .typeId = VOID, .storage = ST_REG_VAL };
            break;
        case EXPR_CAST:
            return emit_cast(expr, frame);
        case EXPR_IF_ELSE:
            return emit_if_else(expr, frame);
        default:
            assert(false);
    }
    return v;
}

// just records for ASM emission after code has been emitted
static void record_def_var(NodeIdx def_var) {
    AstNode *var_node = get_node(def_var);
    assert(var_node->type == AST_DEF_VAR);
    TypeId t = lookup_typeid_from_ast_typename_node(var_node->var_def.typename_);
    
    vec_push(&_ram_vars, &(RamVariable) {
        .size_bytes = get_type(t)->size,
        .symbol_name = var_node->var_def.name
    });
}

static Value emit_fn(NodeIdx fn) {
    AstNode *fn_node = get_node(fn);
    assert(fn_node->type == AST_FN);

    __("%.*s:", (int)fn_node->fn.name.len, fn_node->fn.name.s);

    vec_zero(&_stack_vars);

    // build stackframe of fn arguments
    int bp_offset = 2; // return address at 0(sp), 1(sp)
    for (NodeIdx arg=fn_node->fn.first_arg; arg != 0; arg=get_node(arg)->next_sibling) {
        assert(get_node(arg)->type == AST_FN_ARG);
        
        TypeId argtype = lookup_typeid_from_ast_typename_node(get_node(arg)->fn_arg.typename_);

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
    
    Value ret_val = emit_expression(fn_node->fn.body, frame);
    emit_value_to_register(ret_val, false);
    _i("ret");

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

static void emit_ram_globals() {
    __("\nSECTION \"workram\", WRAM0");
    
    for (int i=0; i<_ram_vars.len; ++i) {
        RamVariable v = *(RamVariable*) vec_get(&_ram_vars, i);
        _i("%.*s: ds %d", (int)v.symbol_name.len, v.symbol_name.s, v.size_bytes);
    }
}

static void init() {
    _stack_vars = vec_init(sizeof(StackVar));
    _ram_vars = vec_init(sizeof(RamVariable));
}

void output_lr35902(NodeIdx root) {
    init();
    init_types();

    output = fopen("out.asm", "w");

    AstNode *root_node = get_node(root);
    assert(root_node->type == AST_MODULE);

    emit_boilerplate();

    for (NodeIdx node=root_node->module.first_child; node != 0; node=get_node(node)->next_sibling) {
        if (get_node(node)->type == AST_FN) {
            emit_fn(node);
        }
        else if (get_node(node)->type == AST_DEF_VAR) {
            record_def_var(node);
        }
        else {
            assert(false);
        }
    }

    // actually need to emit asm for global variables
    emit_ram_globals();

    fclose(output);
}
