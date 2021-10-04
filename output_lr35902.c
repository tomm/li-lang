#include "output_lr35902.h"
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include "str.h"
#include "vec.h"
#include "tokenizer.h"
#include "types.h"
#include "error.h"

static Program *program;
static FILE *output;
static int _local_label_seq = 0;

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

const AstNode *lookup_global_sym(Str name)
{
    Symbol *sym = lookup_program_symbol(program, name);
    if (sym) {
        return get_node(sym->obj);
    } else {
        return NULL;
    }
}

static void emit_boilerplate() {
    if (lookup_global_sym((Str) { .s = "on_vblank", .len = 9 })) {
        __("SECTION \"Vblank\", ROM0[$40]");
        _i("call on_vblank");
        _i("reti");
    }
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

    __("        ld sp, $fffe  ; stack to HRAM (since we are wiping wram)");
    __("        ld hl, $c000");
    __("        ld de, $e000-$c000");
    __("        xor a");
    __("        call __memset");

    __("        ld sp, $e000 ; stack to top of WRAM");
    __("        call main");
    __("    .loop");
    __("        halt");
    __("        nop");
    __("        jr .loop");

    __("__memcpy:");
    __("        ; fn (hl: dest, de: src, bc: length)");
    __("        ld a, [de]");
    __("        ld [hl+], a");
    __("        inc de");
    __("        dec bc");
    __("        ld a, b");
    __("        or c");
    __("        jr nz, __memcpy");
    __("        ret");

    __("__memset:");
    __("        ; fn (hl: dest, de: length, a: value)");
    __("        ld b, a");
    __("    .loop");
    __("        ld [hl+], a");
    __("        dec de");
    __("        ld a, d");
    __("        or a, e");
    __("        ld a, b");
    __("        jp nz, .loop");
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

typedef struct GlobalVariable {
    Str symbol_name;
    int size_bytes;
    bool is_const; /* consts go in ROM */
    NodeIdx value;
} GlobalVariable;

static Vec /*<GlobalVariable>*/ _global_vars;


typedef struct NodeIdxJumpLabel {
    NodeIdx node;
    int label_num;
    int stack_offset; // so we can correct stack before jump (due to temporaries)
} NodeIdxJumpLabel;

static Vec /*<NodeIdxJumpLabel>*/ _jump_labels;

NodeIdxJumpLabel *lookup_jump_label(NodeIdx node) {
    for (int i=0; i<_jump_labels.len; ++i) {
        NodeIdxJumpLabel *l = vec_get(&_jump_labels, i);
        if (l->node == node) return l;
    }
    return NULL;
}

typedef int StackVarIdx;
typedef struct StackVar {
    TypeId type;
    Str ident;
    int offset; // from SP on function entry
    bool is_fn_arg; // false = local
} StackVar;

typedef struct StackFrame {
    int num_vars;
    int stack_offset; // offset caused by temporaries being pushed onto stack
    int locals_top; // stack offset to next allocated local variable
    int locals_size; // total size of local vars
} StackFrame;

Vec /*<StackVar>*/ _stack_vars;

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

/*
static StackVarIdx alloc_stack_var() {
    StackVar v;
    memset(&v, 0, sizeof(StackVar));
    StackVarIdx idx = _stack_vars.len;
    vec_push(&_stack_vars, &v);
    return idx;
}

*/
/* base pointer (bp) is implicit. we only really have a stack pointer */
static StackVarIdx alloc_stack_var(const Token *t, StackFrame frame, StackVar v) {
    if (lookup_stack_var(v.ident, frame) == NULL) {
        StackVarIdx idx = _stack_vars.len;
        vec_push(&_stack_vars, &v);
        return idx;
    } else {
        fatal_error(t, "Local variable called '%.*s' already defined",
                (int)v.ident.len, v.ident.s);
    }
}

static int get_fn_return_type_stack_size(TypeId ret) {
    const Type *t = get_type(ret);
    switch (t->type) {
        case TT_ARRAY:
            return t->size;
        case TT_PTR:
        case TT_PRIM_U8:
        case TT_PRIM_U16:
        case TT_PRIM_VOID:
            // returned in register, so no stack size
            return 0;
        default: assert(false);
    }
}

static int get_call_return_type_stack_size(const AstNode *call) {
    const AstNode *callee = get_node(call->expr.call.callee);
    // consider size of return-by-value non-primitive types (array/struct)
    if (callee->type == AST_EXPR && callee->expr.type == EXPR_IDENT) {
        const AstNode *fn = lookup_global_sym(callee->expr.ident);
        assert(fn->type == AST_FN);
        return get_fn_return_type_stack_size(get_type(fn->fn.type)->func.ret);
    } else {
        // need to handle fn call on expression
        assert(false);
    }
}

static void unalloc_stack_var() {
    StackVar v;
    vec_pop(&_stack_vars, &v);
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
    
    else if (v.typeId == U16 || get_type(v.typeId)->type == TT_PTR) {
        switch (v.storage) {
            case ST_REG_EA:
                _i("ld a, [hl+]");
                _i("ld %s, [hl]", to_aux_reg ? "d" : "h");
                _i("ld %s, a", to_aux_reg ? "e" : "l");
                return (Value) { .typeId = v.typeId, .storage = st };
            case ST_REG_VAL:
                if (st != v.storage) {
                    _i("ld d, h");
                    _i("ld e, l");
                }
                return (Value) { .typeId = v.typeId, .storage = st };
            case ST_REG_VAL_AUX:
                if (st != v.storage) {
                    _i("ld hl, de");
                }
                return (Value) { .typeId = v.typeId, .storage = st };
        }
    }

    else if (get_type(v.typeId)->type == TT_ARRAY) {
        assert(v.storage == ST_REG_EA);
        return v;
    }

    assert(false);
}

static void emit_push_fn_arg(AstNode *n, Value v, StackFrame *frame) {
    const Type *t = get_type(v.typeId);

    switch (t->type) {
        case TT_UNKNOWN:
        case TT_FUNC:
            // should not reach backend
            assert(false);
        case TT_PRIM_VOID:
            fatal_error(n->start_token, "can not use a void value");
            return;
        case TT_PRIM_U8:
            emit_value_to_register(v, false);
            _i("push af");
            frame->stack_offset += 2;
            return;
        case TT_PRIM_U16:
        case TT_PTR:
            emit_value_to_register(v, false);
            _i("push hl");
            frame->stack_offset += 2;
            return;
        case TT_ARRAY:
            /* Passing an array by value */
            assert(v.storage == ST_REG_EA);
            _i("add sp,%d", -t->size);
            _i("ld d, h");
            _i("ld e, l");
            _i("ld hl, sp+0");
            _i("ld bc, %d", t->size);
            _i("call __memcpy");
            frame->stack_offset += t->size;
            return;
    }
}

static void emit_push_temporary(NodeIdx nidx, Value v, StackFrame *frame) {
    AstNode *n = get_node(nidx);
    if (v.typeId == VOID) {
        fatal_error(n->start_token, "can not use a void value");
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
                default:
                    if (get_type(v.typeId)->type == TT_PTR) {
                        _i("push hl");
                        frame->stack_offset += 2;
                        return;
                    } else {
                        assert(false);
                    }
            }
        case ST_REG_EA:
            _i("push hl");
            frame->stack_offset += 2;
            return;
        case ST_REG_VAL_AUX:
            assert(false);
    }
}

static Value emit_pop_temporary(Value v, StackFrame *frame, bool to_aux_reg) {
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
                default:
                    if (get_type(v.typeId)->type == TT_PTR) {
                        _i("pop hl");
                        frame->stack_offset -= 2;
                        return v;
                    } else {
                        assert(false);
                    }
            }
        case ST_REG_EA:
            _i("pop %s", to_aux_reg ? "de" : "hl");
            frame->stack_offset -= 2;
            return (Value) { .storage = ST_REG_EA, .typeId = v.typeId };
    }
    assert(false);
}
static Value emit_expression(NodeIdx expr, StackFrame frame);

typedef struct BuiltinImpl {
    enum BuiltinOp op;
    enum TypeType tt1, tt2;
    Value (*emit)(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2);
} BuiltinImpl;

static void emit_truthy_test_to_zflag(const Token *t, Value v)
{
    if (v.typeId == U8) {
        emit_value_to_register(v, false);  // to 'a' register
        _i("and a, a");
    } /*else if (v.typeId == U16) {
        emit_value_to_register(v, false);  // to 'hl' register
        _i("ld a, l");
        _i("or a, h");
    }*/
    else {
        fatal_error(t, "Type '%.*s' cannot be evaluated for truthyness",
                (int)get_type(v.typeId)->name.len,
                get_type(v.typeId)->name.s);
    }
}

Value emit_assign_u8(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);

    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);
    v2 = emit_value_to_register(v2, true);   // v2 in `b`
    v1 = emit_pop_temporary(v1, frame, false);

    if (v1.storage != ST_REG_EA) {
        fatal_error(get_node(expr)->start_token, "Can not assign to temporary");
    }
    switch (op) {
        case BUILTIN_ASSIGN:
            _i("ld a, b");
            _i("ld [hl], a");
            break;
        case BUILTIN_PLUSASSIGN:
            _i("ld a, [hl]");
            _i("add a, b");
            _i("ld [hl], a");
            break;
        case BUILTIN_MINUSASSIGN:
            _i("ld a, [hl]");
            _i("sub a, b");
            _i("ld [hl], a");
            break;
        case BUILTIN_BITANDASSIGN:
            _i("ld a, [hl]");
            _i("and a, b");
            _i("ld [hl], a");
            break;
        case BUILTIN_BITORASSIGN:
            _i("ld a, [hl]");
            _i("or a, b");
            _i("ld [hl], a");
            break;
        case BUILTIN_BITXORASSIGN:
            _i("ld a, [hl]");
            _i("xor a, b");
            _i("ld [hl], a");
            break;
        case BUILTIN_MULASSIGN:
            _i("ld a, [hl]");
            _i("push hl");
            _i("call __mulu8");
            _i("pop hl");
            _i("ld [hl], a");
            break;
        case BUILTIN_DIVASSIGN:
            _i("ld a, [hl]");
            _i("push hl");
            _i("call __divu8");
            _i("pop hl");
            _i("ld [hl], a");
            break;
        case BUILTIN_MODASSIGN:
            _i("ld a, [hl]");
            _i("push hl");
            _i("call __divu8");
            _i("pop hl");
            _i("ld a, b");
            _i("ld [hl], a");
            break;
        case BUILTIN_LSHIFTASSIGN:
        case BUILTIN_RSHIFTASSIGN:
            {
                const int start_label = _local_label_seq++;
                const int end_label = _local_label_seq++;
                _i("ld c, [hl]");
                _i("ld a, b");
                _label(start_label);
                _i("and a");
                _i("jr z, .l%d", end_label);
                _i("dec a");
                _i("%s c", op == BUILTIN_LSHIFTASSIGN ? "sla" : "srl");
                _i("jr .l%d", start_label);
                _label(end_label);
                _i("ld a, c");
                _i("ld [hl], a");
            }
            break;
        default:
            assert(false);
    }
    return (Value) { .typeId = U8, .storage = ST_REG_VAL };
}

Value emit_ptr_deref(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    Value v = emit_expression(n->expr.builtin.arg1, *frame);
    v = emit_value_to_register(v, false); // v in `hl`
    assert(get_type(v.typeId)->type == TT_PTR);

    // no ASM is emitted. If we have a ptr in ST_REG_VAL, then we have its value in ST_REG_EA :)
    return (Value) { .typeId = get_type(v.typeId)->ptr.ref, .storage = ST_REG_EA };
}

Value emit_addressof(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    Value v = emit_expression(n->expr.builtin.arg1, *frame);

    if (v.storage != ST_REG_EA) {
        fatal_error(get_node(expr)->start_token, "Can not take address of temporary");
    }

    // no ASM is emitted. If we have a value in ST_REG_EA, then we have its pointer in ST_REG_VAL :)
    return (Value) { .typeId = make_ptr_type(v.typeId), .storage = ST_REG_VAL };
}

Value emit_unary_math_u8(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    //AstNode *arg = get_node(n->expr.builtin.arg1);

    Value v = emit_expression(n->expr.builtin.arg1, *frame);

    switch (op) {
        case BUILTIN_UNARY_NEG:
            v = emit_value_to_register(v, true);   // v in `b`
            _i("xor a");
            _i("sub a, b");
            return (Value) { .typeId = U8, .storage = ST_REG_VAL };
        case BUILTIN_UNARY_BITNOT:
            v = emit_value_to_register(v, false);   // v in `a`
            _i("cpl");
            return (Value) { .typeId = U8, .storage = ST_REG_VAL };
        case BUILTIN_UNARY_LOGICAL_NOT:
            {
                const int false_label = _local_label_seq++;
                v = emit_value_to_register(v, false);   // v in `a`
                emit_truthy_test_to_zflag(n->start_token, v);
                _i("ld a, 0");  // can't use xor because it clears flags
                _i("jr nz, .l%d", false_label);
                _i("inc a");
                _label(false_label);
                return (Value) { .typeId = U8, .storage = ST_REG_VAL };
            }
        default:
            assert(false);
    }
}

Value emit_array_indexing_u8(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);

    assert(get_type(v1.typeId)->type == TT_ARRAY);
    assert(v1.storage == ST_REG_EA);
    const Type *contained = get_type(get_type(v1.typeId)->array.contained);

    v2 = emit_value_to_register(v2, true);   // v2 in `b`
    _i("ld e, b");
    _i("ld d, 0");
    if (contained->size != 1) {
        _i("ld hl, %d", contained->size);
        _i("call __mulu16");
        _i("ld d, h");
        _i("ld e, l");
    }
    v1 = emit_pop_temporary(v1, frame, false);
    _i("add hl, de");
    return (Value) { .storage = ST_REG_EA, .typeId = get_type(v1.typeId)->array.contained };
}

Value emit_ptr_addsub_u16(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);

    assert(get_type(v1.typeId)->type == TT_PTR);
    const Type *ref = get_type(get_type(v1.typeId)->ptr.ref);

    v2 = emit_value_to_register(v2, true);   // v2 in `de`
    if (ref->size != 1) {
        _i("ld hl, %d", ref->size);
        _i("call __mulu16");
        _i("ld d, h");
        _i("ld e, l");
    }
    v1 = emit_pop_temporary(v1, frame, false);
    v1 = emit_value_to_register(v1, false);   // v1 in `hl`
    if (op == BUILTIN_ADD) {
        _i("add hl, de");
    } else if (op == BUILTIN_SUB) {
        _i("ld a, l");
        _i("sub a, e");
        _i("ld l, a");

        _i("ld a, h");
        _i("sbc a, d");
        _i("ld h, a");
    } else {
        assert(false);
    }
    return (Value) { .storage = ST_REG_VAL, .typeId = v1.typeId };
}

Value emit_array_indexing_u16(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);

    assert(get_type(v1.typeId)->type == TT_ARRAY);
    assert(v1.storage == ST_REG_EA);
    const Type *contained = get_type(get_type(v1.typeId)->array.contained);

    v2 = emit_value_to_register(v2, true);   // v2 in `de`
    if (contained->size != 1) {
        _i("ld hl, %d", contained->size);
        _i("call __mulu16");
        _i("ld d, h");
        _i("ld e, l");
    }
    v1 = emit_pop_temporary(v1, frame, false);
    _i("add hl, de");
    return (Value) { .storage = ST_REG_EA, .typeId = get_type(v1.typeId)->array.contained };
}

Value emit_logical_and(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    AstNode *arg1 = get_node(n->expr.builtin.arg1);
    AstNode *arg2 = get_node(n->expr.builtin.arg2);

    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_truthy_test_to_zflag(arg1->start_token, v1);
    const int false_label = _local_label_seq++;
    const int end_label = _local_label_seq++;
    _i("jp z, .l%d", false_label);

    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);
    emit_truthy_test_to_zflag(arg2->start_token, v2);
    _i("jp z, .l%d", false_label);
    _i("ld a, 1");
    _i("jr .l%d", end_label);
    _label(false_label);
    _i("xor a");
    _label(end_label);
    return (Value) { .storage = ST_REG_VAL, .typeId = U8 };
}

Value emit_logical_or(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    AstNode *arg1 = get_node(n->expr.builtin.arg1);
    AstNode *arg2 = get_node(n->expr.builtin.arg2);

    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_truthy_test_to_zflag(arg1->start_token, v1);
    const int true_label = _local_label_seq++;
    const int end_label = _local_label_seq++;
    _i("jp nz, .l%d", true_label);

    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);
    emit_truthy_test_to_zflag(arg2->start_token, v2);
    _i("jp nz, .l%d", true_label);
    _i("xor a");
    _i("jr .l%d", end_label);
    _label(true_label);
    _i("ld a, 1");
    _label(end_label);
    return (Value) { .storage = ST_REG_VAL, .typeId = U8 };
}

Value emit_binop_u8(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);

    v2 = emit_value_to_register(v2, true);   // v2 in `b`
    v1 = emit_pop_temporary(v1, frame, false);
    v1 = emit_value_to_register(v1, false);  // v1 in `a`

    // assumes binary op
    switch (op) {
        case BUILTIN_SHIFT_RIGHT:
        case BUILTIN_SHIFT_LEFT:
            {
                const int start_label = _local_label_seq++;
                const int end_label = _local_label_seq++;
                _i("ld c, a");
                _i("ld a, b");
                _label(start_label);
                _i("and a");
                _i("jr z, .l%d", end_label);
                _i("dec a");
                _i("%s c", op == BUILTIN_SHIFT_LEFT ? "sla" : "srl");
                _i("jr .l%d", start_label);
                _label(end_label);
                _i("ld a, c");
            }
            break;
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
        case BUILTIN_DIV:
            _i("call __divu8");
            break;
        case BUILTIN_MODULO:
            _i("call __divu8");
            _i("ld a, b");
            break;
        case BUILTIN_NEQ:
        case BUILTIN_EQ:
            {
                const int false_label = _local_label_seq++;
                _i("cp a, b");
                _i("ld a, 0");
                _i("jr %s, .l%d", op == BUILTIN_EQ ? "nz" : "z", false_label);
                _i("inc a");
                _label(false_label);
            }
            break;
        case BUILTIN_GTE:
        case BUILTIN_LTE:
            {
                const int true_label = _local_label_seq++;
                _i("cp a, b");
                _i("ld a, 1");
                _i("jr %s, .l%d", op == BUILTIN_GTE ? "nc" : "c", true_label);
                _i("jr z, .l%d", true_label);
                _i("xor a");
                _label(true_label);
            }
            break;
        case BUILTIN_LT:
        case BUILTIN_GT:
            {
                const int false_label = _local_label_seq++;
                const char *cflag = op == BUILTIN_GT ? "c" : "nc";
                _i("cp a, b");
                _i("ld a, 0");
                _i("jr z, .l%d", false_label);
                _i("jr %s, .l%d", cflag, false_label);
                _i("inc a");
                _label(false_label);
            }
            break;
        default:
            assert(false);
            break;
    }
    return (Value) { .typeId = U8, .storage = ST_REG_VAL };
}

Value emit_unary_math_u16(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    Value v = emit_expression(n->expr.builtin.arg1, *frame);

    switch (op) {
        case BUILTIN_UNARY_NEG:
            v = emit_value_to_register(v, false); // v in `hl`
            _i("xor a");
            _i("sub a, l");
            _i("ld l, a");
            _i("xor a");
            _i("sbc a, h");
            _i("ld h, a");
            return (Value) { .typeId = U16, .storage = ST_REG_VAL };
        case BUILTIN_UNARY_BITNOT:
            v = emit_value_to_register(v, false); // v in `hl`
            _i("ld a, h");
            _i("cpl");
            _i("ld h, a");
            _i("ld a, l");
            _i("cpl");
            _i("ld l, a");
            return (Value) { .typeId = U16, .storage = ST_REG_VAL };
            /*
        case BUILTIN_UNARY_LOGICAL_NOT:
            {
                const int false_label = _local_label_seq++;
                v = emit_value_to_register(v, false);   // v in `hl`
                emit_truthy_test_to_zflag(n->start_token, v);
                _i("ld a, 0");  // can't use xor because it clears flags
                _i("jr nz, .l%d", false_label);
                _i("inc a");
                _label(false_label);
                return (Value) { .typeId = U8, .storage = ST_REG_VAL };
            }
            */
        default:
            assert(false);
    }
}

Value emit_assign_array(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);
    // v2 (src) in `de`
    _i("ld d, h");
    _i("ld e, l");
    v1 = emit_pop_temporary(v1, frame, false); // v1 (dest) in `hl`

    _i("ld bc, %d", get_type(v1.typeId)->size);
    _i("push hl");
    _i("call __memcpy");
    _i("pop hl");

    return (Value) { .typeId = v1.typeId, .storage = ST_REG_EA };
}

Value emit_assign_u16(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);

    v2 = emit_value_to_register(v2, true);   // v2 in `de`
    v1 = emit_pop_temporary(v1, frame, false);

    if (v1.storage != ST_REG_EA) {
        fatal_error(get_node(expr)->start_token, "Can not assign to temporary");
    }

    switch (op) {
        case BUILTIN_ASSIGN:
            _i("ld a, e");
            _i("ld [hl+], a");
            _i("ld a, d");
            _i("ld [hl-], a");
            break;
        case BUILTIN_PLUSASSIGN:
            _i("ld a, [hl]");
            _i("add a, e");
            _i("ld [hl+], a");

            _i("ld a, [hl]");
            _i("adc a, d");
            _i("ld [hl-], a");
            break;
        case BUILTIN_MINUSASSIGN:
            _i("ld a, [hl]");
            _i("sub a, e");
            _i("ld [hl+], a");

            _i("ld a, [hl]");
            _i("sbc a, d");
            _i("ld [hl-], a");
            break;
        case BUILTIN_BITORASSIGN:
            _i("ld a, [hl]");
            _i("or a, e");
            _i("ld [hl+], a");

            _i("ld a, [hl]");
            _i("or a, d");
            _i("ld [hl-], a");
            break;
        case BUILTIN_BITANDASSIGN:
            _i("ld a, [hl]");
            _i("and a, e");
            _i("ld [hl+], a");

            _i("ld a, [hl]");
            _i("and a, d");
            _i("ld [hl-], a");
            break;
        case BUILTIN_BITXORASSIGN:
            _i("ld a, [hl]");
            _i("xor a, e");
            _i("ld [hl+], a");

            _i("ld a, [hl]");
            _i("xor a, d");
            _i("ld [hl-], a");
            break;
        case BUILTIN_RSHIFTASSIGN:
        case BUILTIN_LSHIFTASSIGN:
            {
                const int start_label = _local_label_seq++;
                const int end_label = _local_label_seq++;
                _i("push hl");
                emit_value_to_register(v1, false);
                // ignore high byte of shift value
                _i("ld a, e");
                _label(start_label);
                _i("and a");
                _i("jr z, .l%d", end_label);
                _i("dec a");
                if (op == BUILTIN_LSHIFTASSIGN) {
                    _i("sla l");
                    _i("rl h");
                } else {
                    _i("srl h");
                    _i("rr l");
                }
                _i("jr .l%d", start_label);
                _label(end_label);
                _i("ld b, h");
                _i("ld a, l");
                _i("pop hl");
                _i("ld [hl+], a");
                _i("ld [hl], b");
                _i("dec hl");
            }
            break;
        case BUILTIN_MULASSIGN:
            _i("push hl");
            emit_value_to_register(v1, false);
            _i("call __mulu16");
            _i("ld d, h");
            _i("ld e, l");
            _i("pop hl");
            _i("ld [hl], e");
            _i("inc hl");
            _i("ld [hl], d");
            _i("dec hl");
            break;
        case BUILTIN_DIVASSIGN:
            _i("push hl");
            emit_value_to_register(v1, false);
            _i("call __divu16");
            _i("ld d, h");
            _i("ld e, l");
            _i("pop hl");
            _i("ld [hl], e");
            _i("inc hl");
            _i("ld [hl], d");
            _i("dec hl");
            break;
        case BUILTIN_MODASSIGN:
            _i("push hl");
            emit_value_to_register(v1, false);
            _i("call __divu16");
            _i("pop hl");
            _i("ld [hl], e");
            _i("inc hl");
            _i("ld [hl], d");
            _i("dec hl");
            break;
        default:
            assert(false);
    }
    // load [hl] to hl, since assignments can be used as values
    // XXX this is inefficient if we aren't using the value
    v1 = emit_value_to_register(v1, false);
    return (Value) { .typeId = v1.typeId, .storage = ST_REG_VAL };
}

Value emit_ptr_opassign_u16(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);

    if (v1.storage != ST_REG_EA) {
        fatal_error(get_node(expr)->start_token, "Can not assign to temporary");
    }

    assert(get_type(v1.typeId)->type == TT_PTR);
    const Type *ref = get_type(get_type(v1.typeId)->ptr.ref);

    v2 = emit_value_to_register(v2, true);   // v2 in `de`
    if (ref->size != 1) {
        _i("ld hl, %d", ref->size);
        _i("call __mulu16");
        _i("ld d, h");
        _i("ld e, l");
    }
    v1 = emit_pop_temporary(v1, frame, false);
    if (op == BUILTIN_PLUSASSIGN) {
        _i("ld a, [hl]");
        _i("add a, e");
        _i("ld [hl+], a");

        _i("ld a, [hl]");
        _i("adc a, d");
        _i("ld [hl-], a");
    } else if (op == BUILTIN_MINUSASSIGN) {
        _i("ld a, [hl]");
        _i("sub a, e");
        _i("ld [hl+], a");

        _i("ld a, [hl]");
        _i("sbc a, d");
        _i("ld [hl-], a");
    } else {
        assert(false);
    }
    return (Value) { .storage = ST_REG_EA, .typeId = v1.typeId };
}

Value emit_binop_u16(NodeIdx expr, StackFrame *frame, enum BuiltinOp op, NodeIdx expr1, NodeIdx expr2) {
    AstNode *n = get_node(expr);
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);

    v2 = emit_value_to_register(v2, true);   // v2 in `de`
    v1 = emit_pop_temporary(v1, frame, false);
    v1 = emit_value_to_register(v1, false);  // v1 in `hl`

    switch (op) {
        case BUILTIN_SHIFT_RIGHT:
        case BUILTIN_SHIFT_LEFT:
            {
                const int start_label = _local_label_seq++;
                const int end_label = _local_label_seq++;
                // ignore high byte of shift value
                _i("ld a, e");
                _label(start_label);
                _i("and a");
                _i("jr z, .l%d", end_label);
                _i("dec a");
                if (op == BUILTIN_SHIFT_LEFT) {
                    _i("sla l");
                    _i("rl h");
                } else {
                    _i("srl h");
                    _i("rr l");
                }
                _i("jr .l%d", start_label);
                _label(end_label);
            }
            break;
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
        case BUILTIN_DIV:
            _i("call __divu16");
            break;
        case BUILTIN_MODULO:
            _i("call __divu16");
            _i("ld h, d");
            _i("ld l, e");
            break;
        case BUILTIN_NEQ:
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

                _i("jr %s, .l%d", op == BUILTIN_EQ ? "nz" : "z", l);
                _i("dec a");
                _label(l);
            }
            return (Value) { .typeId = U8, .storage = ST_REG_VAL };
        case BUILTIN_GT:
        case BUILTIN_LT:
            {
                const int test_lo_label = _local_label_seq++;
                const int end_label = _local_label_seq++;
                const char *cflag = op == BUILTIN_GT ? "c" : "nc";
                // test high byte
                _i("ld a, h");
                _i("cp a, d");
                _i("ld a, 0");
                _i("jr z, .l%d", test_lo_label);
                _i("jr %s, .l%d", cflag, end_label);
                _i("inc a");
                _i("jr .l%d", end_label);
                _label(test_lo_label);
                // test low byte
                _i("ld a, l");
                _i("cp a, e");
                _i("ld a, 0");
                _i("jr z, .l%d", end_label);
                _i("jr %s, .l%d", cflag, end_label);
                _i("inc a");
                _label(end_label);
            }
            return (Value) { .typeId = U8, .storage = ST_REG_VAL };
        case BUILTIN_GTE:
        case BUILTIN_LTE:
            {
                const int test_lo_label = _local_label_seq++;
                const int end_label = _local_label_seq++;
                const char *cflag = op == BUILTIN_GTE ? "nc" : "c";
                // test high byte
                _i("ld a, h");
                _i("cp a, d");
                _i("ld a, 1");
                _i("jr z, .l%d", test_lo_label);
                _i("jr %s, .l%d", cflag, end_label);
                _i("xor a");
                _i("jr .l%d", end_label);
                _label(test_lo_label);
                // test low byte
                _i("ld a, l");
                _i("cp a, e");
                _i("ld a, 1");
                _i("jr z, .l%d", end_label);
                _i("jr %s, .l%d", cflag, end_label);
                _i("xor a");
                _label(end_label);
            }
            return (Value) { .typeId = U8, .storage = ST_REG_VAL };
        default:
            assert(false);
            break;
    }
    return (Value) { .typeId = U16, .storage = ST_REG_VAL };
}

static BuiltinImpl builtin_impls[] = {
    { BUILTIN_UNARY_ADDRESSOF, -1 /* accept any */, TT_PRIM_VOID, emit_addressof },
    { BUILTIN_UNARY_DEREF, TT_PTR, TT_PRIM_VOID, emit_ptr_deref },
    { BUILTIN_ASSIGN, TT_PTR, TT_PTR, emit_assign_u16 },
    { BUILTIN_PLUSASSIGN, TT_PTR, TT_PRIM_U16, emit_ptr_opassign_u16 },
    { BUILTIN_MINUSASSIGN, TT_PTR, TT_PRIM_U16, emit_ptr_opassign_u16 },
    { BUILTIN_ADD, TT_PTR, TT_PRIM_U16, emit_ptr_addsub_u16 },
    { BUILTIN_SUB, TT_PTR, TT_PRIM_U16, emit_ptr_addsub_u16 },

    { BUILTIN_SHIFT_LEFT, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_SHIFT_RIGHT, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_ADD, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_SUB, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_NEQ, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_EQ, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_GT, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_LT, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_GTE, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_LTE, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_MUL, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_DIV, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_MODULO, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_BITXOR, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_BITAND, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_BITOR, TT_PRIM_U8, TT_PRIM_U8, emit_binop_u8 },
    { BUILTIN_ASSIGN, TT_PRIM_U8, TT_PRIM_U8, emit_assign_u8 },
    { BUILTIN_PLUSASSIGN, TT_PRIM_U8, TT_PRIM_U8, emit_assign_u8 },
    { BUILTIN_MINUSASSIGN, TT_PRIM_U8, TT_PRIM_U8, emit_assign_u8 },
    { BUILTIN_MULASSIGN, TT_PRIM_U8, TT_PRIM_U8, emit_assign_u8 },
    { BUILTIN_DIVASSIGN, TT_PRIM_U8, TT_PRIM_U8, emit_assign_u8 },
    { BUILTIN_MODASSIGN, TT_PRIM_U8, TT_PRIM_U8, emit_assign_u8 },
    { BUILTIN_LSHIFTASSIGN, TT_PRIM_U8, TT_PRIM_U8, emit_assign_u8 },
    { BUILTIN_RSHIFTASSIGN, TT_PRIM_U8, TT_PRIM_U8, emit_assign_u8 },
    { BUILTIN_BITANDASSIGN, TT_PRIM_U8, TT_PRIM_U8, emit_assign_u8 },
    { BUILTIN_BITORASSIGN, TT_PRIM_U8, TT_PRIM_U8, emit_assign_u8 },
    { BUILTIN_BITXORASSIGN, TT_PRIM_U8, TT_PRIM_U8, emit_assign_u8 },
    { BUILTIN_ARRAY_INDEXING, TT_ARRAY, TT_PRIM_U8, emit_array_indexing_u8 },
    { BUILTIN_UNARY_NEG, TT_PRIM_U8, TT_PRIM_VOID, emit_unary_math_u8 },
    { BUILTIN_UNARY_BITNOT, TT_PRIM_U8, TT_PRIM_VOID, emit_unary_math_u8 },
    { BUILTIN_UNARY_LOGICAL_NOT, TT_PRIM_U8, TT_PRIM_VOID, emit_unary_math_u8 },

    { BUILTIN_SHIFT_LEFT, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_SHIFT_RIGHT, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_ADD, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_SUB, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_NEQ, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_EQ, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_GT, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_LT, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_GTE, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_LTE, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_MUL, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_DIV, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_MODULO, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_BITXOR, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_BITAND, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_BITOR, TT_PRIM_U16, TT_PRIM_U16, emit_binop_u16 },
    { BUILTIN_ASSIGN, TT_PRIM_U16, TT_PRIM_U16, emit_assign_u16 },
    { BUILTIN_PLUSASSIGN, TT_PRIM_U16, TT_PRIM_U16, emit_assign_u16 },
    { BUILTIN_MINUSASSIGN, TT_PRIM_U16, TT_PRIM_U16, emit_assign_u16 },
    { BUILTIN_MULASSIGN, TT_PRIM_U16, TT_PRIM_U16, emit_assign_u16 },
    { BUILTIN_DIVASSIGN, TT_PRIM_U16, TT_PRIM_U16, emit_assign_u16 },
    { BUILTIN_MODASSIGN, TT_PRIM_U16, TT_PRIM_U16, emit_assign_u16 },
    { BUILTIN_LSHIFTASSIGN, TT_PRIM_U16, TT_PRIM_U16, emit_assign_u16 },
    { BUILTIN_RSHIFTASSIGN, TT_PRIM_U16, TT_PRIM_U16, emit_assign_u16 },
    { BUILTIN_BITANDASSIGN, TT_PRIM_U16, TT_PRIM_U16, emit_assign_u16 },
    { BUILTIN_BITORASSIGN, TT_PRIM_U16, TT_PRIM_U16, emit_assign_u16 },
    { BUILTIN_BITXORASSIGN, TT_PRIM_U16, TT_PRIM_U16, emit_assign_u16 },
    { BUILTIN_ARRAY_INDEXING, TT_ARRAY, TT_PRIM_U16, emit_array_indexing_u16 },
    { BUILTIN_UNARY_NEG, TT_PRIM_U16, TT_PRIM_VOID, emit_unary_math_u16 },
    { BUILTIN_UNARY_BITNOT, TT_PRIM_U16, TT_PRIM_VOID, emit_unary_math_u16 },

    { BUILTIN_LOGICAL_AND, TT_PRIM_U8, TT_PRIM_U8, emit_logical_and },
    { BUILTIN_LOGICAL_OR, TT_PRIM_U8, TT_PRIM_U8, emit_logical_or },

    { BUILTIN_ASSIGN, TT_ARRAY, TT_ARRAY, emit_assign_array },
    { -1 }
};

static Value emit_builtin(NodeIdx call, StackFrame frame) {
    AstNode *n = get_node(call);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_BUILTIN);
    enum BuiltinOp op = n->expr.builtin.op;

    const int n_args = n->expr.builtin.arg2 == 0 ? 1 : 2;

    AstNode *arg1 = get_node(n->expr.builtin.arg1);

    _i("; builtin %s", builtin_name(op));

    enum TypeType ttarg1 = op == BUILTIN_UNARY_ADDRESSOF ? -1 : get_type(arg1->expr.eval_type)->type;
    enum TypeType ttarg2 = n_args > 1 ?  get_type(get_node(n->expr.builtin.arg2)->expr.eval_type)->type : TT_PRIM_VOID;

    // do we have an implementation of this op?
    for (int i=0; builtin_impls[i].op != -1; ++i) {
        if (builtin_impls[i].op == op &&
            builtin_impls[i].tt1 == ttarg1 &&
            builtin_impls[i].tt2 == ttarg2) {
            Value result = builtin_impls[i].emit(call, &frame, op,
                    n->expr.builtin.arg1, n->expr.builtin.arg2);
            if (!is_type_eq(result.typeId, n->expr.eval_type)) {
                fatal_error(n->start_token, "Compiler bug. Builtin operator yielded the wrong type. Expected %.*s but got %.*s",
                        (int)get_type(n->expr.eval_type)->name.len,
                        get_type(n->expr.eval_type)->name.s,
                        (int)get_type(result.typeId)->name.len,
                        get_type(result.typeId)->name.s);
            }
            return result;
        }
    }
    if (n_args == 1) {
        Str typename_ = get_type(arg1->expr.eval_type)->name;
        fatal_error(n->start_token, "LR35902 backend does not support operator '%s' with %.*s argument",
                builtin_name(op),
                (int)typename_.len, typename_.s);
    } else {
        Str typename1 = get_type(arg1->expr.eval_type)->name;
        Str typename2 = get_type(get_node(n->expr.builtin.arg2)->expr.eval_type)->name;
        fatal_error(n->start_token, "LR35902 backend does not support operator '%s' with %.*s and %.*s arguments",
                builtin_name(op),
                (int)typename1.len, typename1.s,
                (int)typename2.len, typename2.s);
    }
}

static Value emit_cast(NodeIdx cast, StackFrame frame) {
    AstNode *n = get_node(cast);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_CAST);

    TypeId to_type = n->expr.cast.to_type;
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
    } else if (get_type(to_type)->type == TT_PTR && get_type(v1.typeId)->type == TT_PTR) {
        // fine. nothing to do
    } else if (v1.typeId == U16 && get_type(to_type)->type == TT_PTR) {
        // fine. nothing to do
    } else if (to_type == U16 && get_type(v1.typeId)->type == TT_PTR) {
        // fine. nothing to do
    } else if (get_type(v1.typeId)->type == TT_ARRAY &&
               get_type(to_type)->type == TT_PTR &&
               is_type_eq(get_type(v1.typeId)->array.contained,
                          get_type(to_type)->ptr.ref)) {
        // fine. nothing to do
    } else {
        assert(false);
    }

    return (Value) { .typeId = to_type, .storage = ST_REG_VAL };
}

static void emit_call_push_args(int arg_num, NodeIdx arg_list_head, StackFrame *frame) {
    // push last to first
    if (arg_list_head == 0) {
        return;
    }

    AstNode *n = get_node(arg_list_head);
    if (n->next_sibling != 0) {
        emit_call_push_args(arg_num+1, n->next_sibling, frame);
    }

    assert(n->type == AST_EXPR);
    Value v = emit_expression(arg_list_head, *frame);
    emit_push_fn_arg(n, v, frame);
}

static Value emit_call(NodeIdx call, StackFrame frame) {
    AstNode *n = get_node(call);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_CALL);

    // is it a built-in op?
    AstNode *callee = get_node(n->expr.call.callee);
    if (callee->type == AST_EXPR && callee->expr.type == EXPR_IDENT) {
        const AstNode *fn = lookup_global_sym(callee->expr.ident);

        // actualy compile error emitted by program.c
        assert(fn != NULL);
        assert(fn->type == AST_FN);

        const int old_stack = frame.stack_offset;
        emit_call_push_args(0, n->expr.call.first_arg, &frame);
        _i("call %.*s", (int)callee->expr.ident.len, callee->expr.ident.s);
        const int stack_correction = frame.stack_offset - old_stack;
        if (stack_correction) {
            _i("add sp, %d", stack_correction);
        }
        frame.stack_offset += stack_correction;

        Type *fntype = get_type(fn->fn.type);
        Type *rettype = get_type(fntype->func.ret);
        assert(fntype->type == TT_FUNC);
        return (Value) { .typeId = fntype->func.ret, .storage = rettype->type == TT_ARRAY ? ST_REG_EA : ST_REG_VAL };
    } else {
        fatal_error(callee->start_token, "fn call by expression not implemented");
    }
}

static Value emit_loop(NodeIdx expr, StackFrame frame) {
    AstNode *n = get_node(expr);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_LOOP);

    const int jump_label = _local_label_seq++;

    vec_push(&_jump_labels, &(NodeIdxJumpLabel) {
        .node = expr,
        .label_num = jump_label,
        .stack_offset = frame.stack_offset,
    });

    const NodeIdx on_next_iter = n->expr.loop.on_next_iter;

    if (on_next_iter) {
        _i("jp .l%d_cond", jump_label);
        __(".l%d_continue:", jump_label);
        emit_expression(n->expr.loop.on_next_iter, frame);
        __(".l%d_cond:", jump_label);
    } else {
        __(".l%d_continue:", jump_label);
    }

    if (n->expr.loop.condition != 0) {
        Value condition = emit_expression(n->expr.loop.condition, frame);
        emit_truthy_test_to_zflag(get_node(n->expr.loop.condition)->start_token, condition);
        _i("jp z, .l%d_break", jump_label);
    }
    /*Value body =*/ emit_expression(n->expr.loop.body, frame);
    _i("jp .l%d_continue", jump_label);
    __(".l%d_break:", jump_label);

    return (Value) { .typeId = VOID, .storage = ST_REG_VAL };
}

static Value emit_if_else(NodeIdx expr, StackFrame frame) {
    AstNode *n = get_node(expr);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_IF_ELSE);

    Value condition = emit_expression(n->expr.if_else.condition, frame);

    const int else_label = _local_label_seq++;
    const int end_label = _local_label_seq++;

    emit_truthy_test_to_zflag(get_node(n->expr.if_else.condition)->start_token, condition);
    _i("jp z, .l%d", else_label);

    Value on_true = emit_expression(n->expr.if_else.on_true, frame);

    if (n->expr.if_else.on_false != 0) {
        _i("jp .l%d", end_label);
        _label(else_label);

        Value on_false = emit_expression(n->expr.if_else.on_false, frame);

        if (!is_type_eq(on_false.typeId, on_true.typeId)) {
            fatal_error(n->start_token, "if-else expects both branches to evaluate to the same type. found %.*s and %.*s",
                    (int)get_type(on_true.typeId)->name.len,
                    get_type(on_true.typeId)->name.s,
                    (int)get_type(on_false.typeId)->name.len,
                    get_type(on_false.typeId)->name.s);
        }

        _label(end_label);
        // the storage class must be the same
        if (on_false.storage != on_true.storage) {
            fatal_error(n->start_token, "storage class of if branches differs. THIS CODE IS UNFINISHED. giving up");
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
        const int type_weirdness_offset = var->is_fn_arg ? get_type(var->type)->stack_offset : 0;

        _i("ld hl, sp%+d", var->offset + frame.stack_offset + type_weirdness_offset);
        return (Value) { .typeId = var->type, .storage = ST_REG_EA };
    }

    const AstNode *global = lookup_global_sym(n->expr.ident);

    if (global) {
        if (global->type != AST_DEF_VAR) {
            fatal_error(n->start_token, "%.*s is not a variable",
                    (int)n->expr.ident.len, n->expr.ident.s);
        }
        _i("ld hl, %.*s", (int)n->expr.ident.len, n->expr.ident.s);
        return (Value) { .typeId = global->var_def.type, .storage = ST_REG_EA };
    }

    fatal_error(n->start_token, "Variable '%.*s' is not defined", (int)n->expr.ident.len, n->expr.ident.s);
}

static Value emit_local_scope(NodeIdx scope, StackFrame frame) {
    const AstNode *n = get_node(scope);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_LOCAL_SCOPE);

    /*StackVarIdx var =*/ alloc_stack_var(n->start_token, frame, (StackVar) {
        .type = n->expr.local_scope.var_type,
        .ident = n->expr.local_scope.var_name,
        .offset = frame.locals_top,
        .is_fn_arg = false
    });
    frame.locals_top += get_type(n->expr.local_scope.var_type)->size;
    frame.num_vars++;
    Value v = emit_expression(get_node(scope)->expr.local_scope.scoped_expr, frame);
    unalloc_stack_var();
    return v;
}

static Value emit_return(NodeIdx val, StackFrame frame) {
    Value ret_val = emit_expression(val, frame);
    emit_value_to_register(ret_val, false);

    if (frame.locals_size) {
        _i("add sp, %d", frame.locals_size);
    }

    _i("ret");
    return ret_val;
}

static Value emit_expression(NodeIdx expr, StackFrame frame) {
    Value v = (Value) { .typeId = VOID, .storage = ST_REG_VAL };
    AstNode *n = get_node(expr);
    assert(n->type == AST_EXPR);

    switch (n->expr.type) {
        case EXPR_GOTO:
            {
                NodeIdxJumpLabel *l = lookup_jump_label(n->expr.goto_.target);
                assert(l != NULL);
                const int stack_correction = frame.stack_offset - l->stack_offset;
                if (stack_correction) {
                    _i("add sp, %d", stack_correction);
                }
                if (n->expr.goto_.is_continue) {
                    _i("jp .l%d_continue", l->label_num);
                } else {
                    _i("jp .l%d_break", l->label_num);
                }
            }
            // XXX should be never type
            return (Value) { .typeId = VOID, .storage = ST_REG_VAL };
        case EXPR_BUILTIN:
            return emit_builtin(expr, frame);
        case EXPR_ASM:
            fprintf(output, "\n%.*s\n", (int)n->expr.asm_.asm_text.len, n->expr.asm_.asm_text.s);
            return (Value) { .typeId = VOID, .storage = ST_REG_VAL };
        case EXPR_CALL:
            return emit_call(expr, frame);
        case EXPR_LIST:
            for (NodeIdx e=n->expr.list.first_child; e != 0; e=get_node(e)->next_sibling) {
                v = emit_expression(e, frame);
            }
            break;
        case EXPR_IDENT:
            return emit_identifier(expr, frame);
        case EXPR_LITERAL:
            switch (n->expr.literal.type) {
                case LIT_INT_ANY:
                    // should not reach backend
                    assert(false);
                case LIT_U8:
                    _i("ld a, $%x", n->expr.literal.literal_int);
                    v = (Value) { .typeId = U8, .storage = ST_REG_VAL };
                    break;
                case LIT_U16:
                    _i("ld hl, $%x", n->expr.literal.literal_int);
                    v = (Value) { .typeId = U16, .storage = ST_REG_VAL };
                    break;
                case LIT_VOID:
                    // the semicolon at the end of a list of expressions :)
                    v = (Value) { .typeId = VOID, .storage = ST_REG_VAL };
                    break;
                case LIT_STR:
                    {
                        vec_push(&_global_vars, &(GlobalVariable) {
                            .size_bytes = get_type(n->expr.eval_type)->size,
                            .symbol_name = { .s = 0 },
                            .is_const = true,
                            .value = expr
                        });
                        _i("ld hl, __L%d", expr);
                    }
                    v = (Value) { .typeId = n->expr.eval_type, .storage = ST_REG_EA };
                    break;
                case LIT_ARRAY:
                    assert(false);
                    break;
            }
            break;
        case EXPR_CAST:
            return emit_cast(expr, frame);
        case EXPR_IF_ELSE:
            return emit_if_else(expr, frame);
        case EXPR_LOOP:
            return emit_loop(expr, frame);
        case EXPR_LOCAL_SCOPE:
            return emit_local_scope(expr, frame);
        case EXPR_RETURN:
            return emit_return(n->expr.return_.val, frame);
        default:
            assert(false);
    }
    return v;
}

// just records for ASM emission after code has been emitted
static void record_def_var(NodeIdx def_var) {
    AstNode *var_node = get_node(def_var);
    assert(var_node->type == AST_DEF_VAR);
    TypeId t = var_node->var_def.type;

    if (!var_node->var_def.is_const && var_node->var_def.value) {
        fatal_error(var_node->start_token, "LR35902 backend does not support initialization of RAM globals");
    }
    
    vec_push(&_global_vars, &(GlobalVariable) {
        .size_bytes = get_type(t)->size,
        .symbol_name = var_node->var_def.name,
        .is_const = var_node->var_def.is_const,
        .value = var_node->var_def.value
    });
}

int max(int a, int b) { return a>b?a:b; }

static int get_max_local_vars_size(NodeIdx n)
{
    if (n == 0) return 0;
    const AstNode *node = get_node(n);

    assert(node->type == AST_EXPR);

    int size = 0;

    switch (node->expr.type) {
        case EXPR_ASM:
            break;
        case EXPR_LOCAL_SCOPE:
            size = get_type(node->expr.local_scope.var_type)->size +
                   get_max_local_vars_size(node->expr.local_scope.scoped_expr);
            break;
        case EXPR_RETURN:
            size = max(size, get_max_local_vars_size(node->expr.return_.val));
            break;
        case EXPR_IF_ELSE:
            size = max(size, get_max_local_vars_size(node->expr.if_else.condition));
            size = max(size, get_max_local_vars_size(node->expr.if_else.on_true));
            size = max(size, get_max_local_vars_size(node->expr.if_else.on_false));
            break;
        case EXPR_LIST:
            for (int c=node->expr.list.first_child; c!=0; c=get_node(c)->next_sibling) {
                size = max(size, get_max_local_vars_size(c));
            }
            break;
        case EXPR_IDENT:
            break;
        case EXPR_LITERAL:
            break;
        case EXPR_CALL:
            for (int c=node->expr.call.first_arg; c!=0; c=get_node(c)->next_sibling) {
                size = max(size, get_max_local_vars_size(c));
            }
            break;
        case EXPR_BUILTIN:
            size = max(size, get_max_local_vars_size(node->expr.builtin.arg1));
            if (node->expr.builtin.arg2 != 0) {
                size = max(size, get_max_local_vars_size(node->expr.builtin.arg2));
            }
            break;
        case EXPR_CAST:
            size = max(size, get_max_local_vars_size(node->expr.cast.arg));
            break;
        case EXPR_LOOP:
            if (node->expr.loop.condition) {
                size = max(size, get_max_local_vars_size(node->expr.loop.condition));
            }
            size = max(size, get_max_local_vars_size(node->expr.loop.body));
            if (node->expr.loop.on_next_iter) {
                size = max(size, get_max_local_vars_size(node->expr.loop.on_next_iter));
            }
            break;
        case EXPR_GOTO:
            break;
        default:
            assert(false);
    }
    return size;
}

static Value emit_fn(NodeIdx fn) {
    AstNode *fn_node = get_node(fn);
    assert(fn_node->type == AST_FN);

    __("%.*s:", (int)fn_node->fn.name.len, fn_node->fn.name.s);

    /* Allocate enough stack space for the maximum local
     * scope size of this function (requires walking the function
     * body expression tree to find local scopes */
    const int local_vars_bytes = get_max_local_vars_size(fn_node->fn.body);

    // because of instruction used to make stack space... XXX
    assert(local_vars_bytes < 128);

    if (local_vars_bytes) {
        _i("add sp, %d", -local_vars_bytes);
    }

    vec_zero(&_stack_vars);

    // build stackframe of fn arguments
    int bp_offset = 2; // return address at 0(sp), 1(sp)
    bp_offset += local_vars_bytes;

    StackFrame frame = { .num_vars = 0, .stack_offset = 0, .locals_top = 0, .locals_size = local_vars_bytes };

    for (NodeIdx arg=fn_node->fn.first_arg; arg != 0; arg=get_node(arg)->next_sibling) {
        assert(get_node(arg)->type == AST_FN_ARG);
        
        TypeId argtype = get_node(arg)->fn_arg.type;

        /*StackVarIdx v =*/ alloc_stack_var(get_node(arg)->start_token, frame, (StackVar) {
            .type = argtype,
            .ident = get_node(arg)->fn_arg.name,
            .offset = bp_offset,
            .is_fn_arg = true
        });
        bp_offset += get_type(argtype)->stack_size;
        frame.num_vars++;
    }

    /*
    for (int i=0; i<_stack_vars.len; ++i) {
        StackVar *v = get_stack_var(i);
        fprintf(stderr, "var %.*s: %d(bp)\n", (int)v->ident.len, v->ident.s, v->offset);
    }
    */
    
    return emit_return(fn_node->fn.body, frame);
}

static void _emit_const(NodeIdx node) {
    AstNode *n = get_node(node);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_LITERAL);

    switch (n->expr.literal.type) {
        case LIT_U8:
            _i("db %d", n->expr.literal.literal_int & 0xff);
            break;
        case LIT_U16:
            _i("dw %d", n->expr.literal.literal_int & 0xffff);
            break;
        case LIT_VOID:
            break;
        case LIT_STR:
            _i("db \"%.*s\", 0",
                    n->expr.literal.literal_str.len,
                    n->expr.literal.literal_str.s);
            break;
        case LIT_ARRAY:
            for (int child=n->expr.literal.literal_array_first_val; child!=0; child=get_node(child)->next_sibling) {
                _emit_const(child);
            }
            break;
    }
}

static void emit_rom_globals() {
    for (int i=0; i<_global_vars.len; ++i) {
        GlobalVariable v = *(GlobalVariable*) vec_get(&_global_vars, i);
        if (v.is_const) {
            if (v.symbol_name.s) {
                __("%.*s:", v.symbol_name.len, v.symbol_name.s);
            } else {
                __("__L%d:", v.value /* is NodeIdx */);
            }
            _emit_const(v.value);
        }
    }
}
static void emit_ram_globals() {
    __("\nSECTION \"workram\", WRAM0");
    
    for (int i=0; i<_global_vars.len; ++i) {
        GlobalVariable v = *(GlobalVariable*) vec_get(&_global_vars, i);
        if (!v.is_const) {
            __("%.*s:", v.symbol_name.len, v.symbol_name.s);
            _i("ds %d", v.size_bytes);
        }
    }
}

static void init() {
    _stack_vars = vec_init(sizeof(StackVar));
    _global_vars = vec_init(sizeof(GlobalVariable));
    _jump_labels = vec_init(sizeof(NodeIdxJumpLabel));
}

void output_lr35902(Program *prog) {
    init();

    program = prog;
    output = fopen("out.asm", "w");

    AstNode *root_node = get_node(prog->root);
    assert(root_node->type == AST_MODULE);

    emit_boilerplate();

    for (NodeIdx node=root_node->module.first_child; node != 0; node=get_node(node)->next_sibling) {
        AstNode *n = get_node(node);
        if (n->type == AST_FN) {
            if (n->fn.body != 0) {
                vec_zero(&_jump_labels);
                emit_fn(node);
            }
        }
        else if (n->type == AST_DEF_VAR) {
            record_def_var(node);
        }
        else if (n->type == AST_EXPR && n->expr.type == EXPR_ASM) {
            fprintf(output, "\n%.*s\n", (int)n->expr.asm_.asm_text.len, n->expr.asm_.asm_text.s);
        } else {
            assert(false);
        }
    }

    // actually need to emit asm for global variables
    emit_rom_globals();
    emit_ram_globals();

    fclose(output);
}
