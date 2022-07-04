#include "output_lr35902.h"
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include <stdint.h>
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
        return sym->obj;
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

    __("__memcmp:");
    __("        ; fn (hl: dest, de: src, bc: length) -> bool (0 or 1)");
    __("        ld a, b");
    __("        or c");
    __("        jr z, .eq");
    __("        ld a, [de]");
    __("        cp [hl]");
    __("        jr nz, .neq");
    __("        inc de");
    __("        inc hl");
    __("        dec bc");
    __("        jr __memcmp");
    __("  .eq:  ld a, 1");
    __("        ret");
    __("  .neq: xor a");
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
 *  U8/I8         hl          a              b
 *  U16/I16       hl         hl             de
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
    AstNode *value;
} GlobalVariable;

static Vec /*<GlobalVariable>*/ _global_vars;


typedef struct AstNodeJumpLabel {
    AstNode *node;
    int label_num;
    int stack_offset; // so we can correct stack before jump (due to temporaries)
} AstNodeJumpLabel;

static Vec /*<AstNodeJumpLabel>*/ _jump_labels;

AstNodeJumpLabel *lookup_jump_label(AstNode *node) {
    for (int i=0; i<_jump_labels.len; ++i) {
        AstNodeJumpLabel *l = vec_get(&_jump_labels, i);
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
        assert(false);
    }
}

/*
static int get_fn_return_type_stack_size(TypeId ret) {
    const Type *t = get_type(ret);
    switch (t->type) {
        case TT_ARRAY:
            return t->size;
        case TT_PTR:
        case TT_PRIM_U8:
        case TT_PRIM_I8:
        case TT_PRIM_U16:
        case TT_PRIM_I16:
        case TT_PRIM_VOID:
            // returned in register, so no stack size
            return 0;
        default: assert(false);
    }
}

static int get_call_return_type_stack_size(const AstNode *call) {
    const AstNode *callee = call->expr.call.callee;
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
*/

static void unalloc_stack_var() {
    StackVar v;
    vec_pop(&_stack_vars, &v);
}

static Value emit_value_to_register(Value v, bool to_aux_reg) {
    const enum Storage st = to_aux_reg ? ST_REG_VAL_AUX : ST_REG_VAL;

    if (v.typeId == VOID) {
        return (Value) { .typeId = VOID, .storage = ST_REG_VAL };
    }

    else if (v.typeId == U8 || v.typeId == I8 || v.typeId == BOOL) {
        switch (v.storage) {
            case ST_REG_EA:
                _i("ld %s, [hl]", to_aux_reg ? "b" : "a");
                return (Value) { .typeId = v.typeId, .storage = st };
            case ST_REG_VAL:
                if (st != v.storage) {
                    _i("ld b, a");
                }
                return (Value) { .typeId = v.typeId, .storage = st };
            case ST_REG_VAL_AUX:
                if (st != v.storage) {
                    _i("ld a, b");
                }
                return (Value) { .typeId = v.typeId, .storage = st };
        }
    }
    
    else if (v.typeId == U16 || v.typeId == I16 || get_type(v.typeId)->type == TT_PTR) {
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

    else if (get_type(v.typeId)->type == TT_STRUCT) {
        assert(v.storage == ST_REG_EA);
        return v;
    }

    assert(false);
}

static void emit_push_fn_arg(AstNode *n, Value v, StackFrame *frame) {
    const Type *t = get_type(v.typeId);

    switch (t->type) {
        case TT_UNKNOWN:
            // should not reach backend
            assert(false);
        case TT_FUNC:
            // should not reach backend
            assert(false);
        case TT_PRIM_VOID:
            assert(false);
            return;
        case TT_PRIM_BOOL:
            emit_value_to_register(v, false);
            _i("push af");
            frame->stack_offset += 2;
            return;
        case TT_PRIM_U8:
        case TT_PRIM_I8:
            emit_value_to_register(v, false);
            _i("push af");
            frame->stack_offset += 2;
            return;
        case TT_PRIM_U16:
        case TT_PRIM_I16:
        case TT_PTR:
            emit_value_to_register(v, false);
            _i("push hl");
            frame->stack_offset += 2;
            return;
        case TT_ARRAY:
        case TT_STRUCT:
            /* Passing an array or struct by value */
            assert(v.storage == ST_REG_EA);
            _i("add sp,%d", -t->size);
            _i("ld d, h");
            _i("ld e, l");
            _i("ld hl, sp+0");
            _i("ld bc, %d", t->size);
            _i("call __memcpy");
            frame->stack_offset += t->size;
            return;
        case TT_NEVER:
            // or should we just do nothing
            assert(false);
    }
}

static void emit_push_temporary(AstNode *nidx, Value v, StackFrame *frame) {
    assert(v.typeId != VOID);
    switch (v.storage) {
        case ST_REG_VAL:
            switch (v.typeId) {
                case BOOL:
                    _i("push af");
                    frame->stack_offset += 2;
                    return;
                case U8:
                case I8:
                    _i("push af");
                    frame->stack_offset += 2;
                    return;
                case U16:
                case I16:
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
                case BOOL:
                    _i("pop %s", to_aux_reg ? "bc" : "af");
                    frame->stack_offset -= 2;
                    return (Value) { .storage = st, .typeId = BOOL };
                case U8:
                case I8:
                    _i("pop %s", to_aux_reg ? "bc" : "af");
                    frame->stack_offset -= 2;
                    return (Value) { .storage = st, .typeId = v.typeId };
                case U16:
                case I16:
                    _i("pop %s", to_aux_reg ? "de" : "hl");
                    frame->stack_offset -= 2;
                    return (Value) { .storage = st, .typeId = v.typeId };
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
static Value emit_expression(AstNode *expr, StackFrame frame);

static void emit_bool_to_z_flag(Value v)
{
    assert(v.typeId == BOOL);
    emit_value_to_register(v, false);  // to 'a' register
    _i("and a, a");
}

Value emit_assign_8(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);
    v2 = emit_value_to_register(v2, true);   // v2 in `b`
    v1 = emit_pop_temporary(v1, frame, false);

    if (v1.storage != ST_REG_EA) {
        fatal_error(n->start_token, "Can not assign to temporary");
    }
    switch (op) {
        case OP_ASSIGN_8:
            _i("ld a, b");
            _i("ld [hl], a");
            break;
        case OP_ADD_ASSIGN_8:
            _i("ld a, [hl]");
            _i("add a, b");
            _i("ld [hl], a");
            break;
        case OP_SUB_ASSIGN_8:
            _i("ld a, [hl]");
            _i("sub a, b");
            _i("ld [hl], a");
            break;
        case OP_AND_ASSIGN_8:
            _i("ld a, [hl]");
            _i("and a, b");
            _i("ld [hl], a");
            break;
        case OP_OR_ASSIGN_8:
            _i("ld a, [hl]");
            _i("or a, b");
            _i("ld [hl], a");
            break;
        case OP_XOR_ASSIGN_8:
            _i("ld a, [hl]");
            _i("xor a, b");
            _i("ld [hl], a");
            break;
        case OP_MUL_ASSIGN_8:
            _i("ld a, [hl]");
            _i("push hl");
            _i("call __mul8");
            _i("pop hl");
            _i("ld [hl], a");
            break;
        case OP_DIV_ASSIGN_U8:
            _i("ld a, [hl]");
            _i("push hl");
            _i("call __divu8");
            _i("pop hl");
            _i("ld [hl], a");
            break;
        case OP_MOD_ASSIGN_U8:
            _i("ld a, [hl]");
            _i("push hl");
            _i("call __divu8");
            _i("pop hl");
            _i("ld a, b");
            _i("ld [hl], a");
            break;
        case OP_DIV_ASSIGN_I8:
            _i("ld a, [hl]");
            _i("push hl");
            _i("call __divi8");
            _i("pop hl");
            _i("ld [hl], a");
            break;
        case OP_MOD_ASSIGN_I8:
            _i("ld a, [hl]");
            _i("push hl");
            _i("call __divi8");
            _i("pop hl");
            _i("ld a, b");
            _i("ld [hl], a");
            break;
        case OP_LSL_ASSIGN_8:
        case OP_LSR_ASSIGN_8:
        case OP_ASR_ASSIGN_8:
            {
                const int start_label = _local_label_seq++;
                const int end_label = _local_label_seq++;
                _i("ld c, [hl]");
                _i("ld a, b");
                _label(start_label);
                _i("and a");
                _i("jr z, .l%d", end_label);
                _i("dec a");
                _i("%s c", op == OP_LSL_ASSIGN_8
                        ? "sla"
                        : op == OP_LSR_ASSIGN_8
                        ? "srl"
                        : op == OP_ASR_ASSIGN_8
                        ? "sra"
                        : (assert(false), "?"));
                _i("jr .l%d", start_label);
                _label(end_label);
                _i("ld a, c");
                _i("ld [hl], a");
            }
            break;
        default:
            assert(false);
    }
    return (Value) { .typeId = v1.typeId, .storage = ST_REG_VAL };
}

Value emit_ptr_deref(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v = emit_expression(n->expr.builtin.arg1, *frame);
    v = emit_value_to_register(v, false); // v in `hl`
    assert(get_type(v.typeId)->type == TT_PTR);

    // no ASM is emitted. If we have a ptr in ST_REG_VAL, then we have its value in ST_REG_EA :)
    return (Value) { .typeId = get_type(v.typeId)->ptr.ref, .storage = ST_REG_EA };
}

Value emit_addressof(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v = emit_expression(n->expr.builtin.arg1, *frame);

    if (v.storage != ST_REG_EA) {
        fatal_error(n->start_token, "Can not take address of temporary");
    }

    // no ASM is emitted. If we have a value in ST_REG_EA, then we have its pointer in ST_REG_VAL :)
    return (Value) { .typeId = make_ptr_type(v.typeId), .storage = ST_REG_VAL };
}

Value emit_logical_not(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *_) {
    Value v = emit_expression(n->expr.builtin.arg1, *frame);

    const int false_label = _local_label_seq++;
    v = emit_value_to_register(v, false);   // v in `a`
    emit_bool_to_z_flag(v);
    _i("ld a, 0");  // can't use xor because it clears flags
    _i("jr nz, .l%d", false_label);
    _i("inc a");
    _label(false_label);
    return (Value) { .typeId = BOOL, .storage = ST_REG_VAL };
}

Value emit_unary_op_8(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v = emit_expression(n->expr.builtin.arg1, *frame);

    switch (op) {
        case OP_UNARY_NEG_8:
            v = emit_value_to_register(v, true);   // v in `b`
            _i("xor a");
            _i("sub a, b");
            return (Value) { .typeId = v.typeId, .storage = ST_REG_VAL };
        case OP_NOT_8:
            v = emit_value_to_register(v, false);   // v in `a`
            _i("cpl");
            return (Value) { .typeId = v.typeId, .storage = ST_REG_VAL };
        default:
            assert(false);
    }
}

Value emit_array_indexing_u8(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
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
        _i("call __mul16");
        _i("ld d, h");
        _i("ld e, l");
    }
    v1 = emit_pop_temporary(v1, frame, false);
    _i("add hl, de");
    return (Value) { .storage = ST_REG_EA, .typeId = get_type(v1.typeId)->array.contained };
}

Value emit_ptr_addsub_u16(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);

    assert(get_type(v1.typeId)->type == TT_PTR);
    const Type *ref = get_type(get_type(v1.typeId)->ptr.ref);

    v2 = emit_value_to_register(v2, true);   // v2 in `de`
    if (ref->size != 1) {
        _i("ld hl, %d", ref->size);
        _i("call __mul16");
        _i("ld d, h");
        _i("ld e, l");
    }
    v1 = emit_pop_temporary(v1, frame, false);
    v1 = emit_value_to_register(v1, false);   // v1 in `hl`
    if (op == OP_PTR_ADD) {
        _i("add hl, de");
    } else if (op == OP_PTR_SUB) {
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

Value emit_array_indexing_u16(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);

    assert(get_type(v1.typeId)->type == TT_ARRAY);
    assert(v1.storage == ST_REG_EA);
    const Type *contained = get_type(get_type(v1.typeId)->array.contained);

    v2 = emit_value_to_register(v2, true);   // v2 in `de`
    if (contained->size != 1) {
        _i("ld hl, %d", contained->size);
        _i("call __mul16");
        _i("ld d, h");
        _i("ld e, l");
    }
    v1 = emit_pop_temporary(v1, frame, false);
    _i("add hl, de");
    return (Value) { .storage = ST_REG_EA, .typeId = get_type(v1.typeId)->array.contained };
}

Value emit_logical_and(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_bool_to_z_flag(v1);
    const int false_label = _local_label_seq++;
    const int end_label = _local_label_seq++;
    _i("jp z, .l%d", false_label);

    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);
    emit_bool_to_z_flag(v2);
    _i("jp z, .l%d", false_label);
    _i("ld a, 1");
    _i("jr .l%d", end_label);
    _label(false_label);
    _i("xor a");
    _label(end_label);
    return (Value) { .storage = ST_REG_VAL, .typeId = BOOL };
}

Value emit_logical_or(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_bool_to_z_flag(v1);
    const int true_label = _local_label_seq++;
    const int end_label = _local_label_seq++;
    _i("jp nz, .l%d", true_label);

    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);
    emit_bool_to_z_flag(v2);
    _i("jp nz, .l%d", true_label);
    _i("xor a");
    _i("jr .l%d", end_label);
    _label(true_label);
    _i("ld a, 1");
    _label(end_label);
    return (Value) { .storage = ST_REG_VAL, .typeId = BOOL };
}

Value emit_binop_u8(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);

    v2 = emit_value_to_register(v2, true);   // v2 in `b`
    v1 = emit_pop_temporary(v1, frame, false);
    v1 = emit_value_to_register(v1, false);  // v1 in `a`

    // assumes binary op
    switch (op) {
        case OP_ASR_8:
        case OP_LSR_8:
        case OP_LSL_8:
            {
                const int start_label = _local_label_seq++;
                const int end_label = _local_label_seq++;
                _i("ld c, a");
                _i("ld a, b");
                _label(start_label);
                _i("and a");
                _i("jr z, .l%d", end_label);
                _i("dec a");
                _i("%s c", op == OP_LSL_8
                        ? "sla"
                        : op == OP_LSR_8
                        ? "srl"
                        : op == OP_ASR_8
                        ? "sra"
                        : (assert(false), "?"));
                _i("jr .l%d", start_label);
                _label(end_label);
                _i("ld a, c");
            }
            break;
        case OP_ADD_8:
            _i("add a, b");
            break;
        case OP_SUB_8:
            _i("sub a, b");
            break;
        case OP_OR_8:
            _i("or a, b");
            break;
        case OP_AND_8:
            _i("and a, b");
            break;
        case OP_XOR_8:
            _i("xor a, b");
            break;
        case OP_MUL_8:
            _i("call __mul8");
            break;
        case OP_DIV_U8:
            _i("call __divu8");
            break;
        case OP_MOD_U8:
            _i("call __divu8");
            _i("ld a, b");
            break;
        case OP_DIV_I8:
            _i("call __divi8");
            break;
        case OP_MOD_I8:
            _i("call __divi8");
            _i("ld a, b");
            break;
        case OP_NEQ_8:
        case OP_EQ_8:
            {
                const int false_label = _local_label_seq++;
                _i("cp a, b");
                _i("ld a, 0");
                _i("jr %s, .l%d", op == OP_EQ_8 ? "nz" : "z", false_label);
                _i("inc a");
                _label(false_label);
            }
            return (Value) { .typeId = BOOL, .storage = ST_REG_VAL };
        case OP_GTE_U8:
        case OP_LTE_U8:
            {
                const int true_label = _local_label_seq++;
                _i("cp a, b");
                _i("ld a, 1");
                _i("jr %s, .l%d", op == OP_GTE_U8 ? "nc" : "c", true_label);
                _i("jr z, .l%d", true_label);
                _i("xor a");
                _label(true_label);
            }
            return (Value) { .typeId = BOOL, .storage = ST_REG_VAL };
        case OP_LTE_I8:
        case OP_GTE_I8:
            {
                const int true_label = _local_label_seq++;
                const char *nflag = op == OP_GTE_I8 ? "z" : "nz";
                _i("sub a, b");
                _i("ld b, 1");
                _i("jr z, .l%d", true_label);
                _i("bit 7, a");
                _i("jr %s, .l%d", nflag, true_label);
                _i("dec b");
                _label(true_label);
                _i("ld a, b");
            }
            return (Value) { .typeId = BOOL, .storage = ST_REG_VAL };
        case OP_LT_U8:
        case OP_GT_U8:
            {
                const int false_label = _local_label_seq++;
                const char *cflag = op == OP_GT_U8 ? "c" : "nc";
                _i("cp a, b");
                _i("ld a, 0");
                _i("jr z, .l%d", false_label);
                _i("jr %s, .l%d", cflag, false_label);
                _i("inc a");
                _label(false_label);
            }
            return (Value) { .typeId = BOOL, .storage = ST_REG_VAL };
        case OP_LT_I8:
        case OP_GT_I8:
            {
                const int false_label = _local_label_seq++;
                const char *nflag = op == OP_GT_I8 ? "nz" : "z";
                _i("sub a, b");
                _i("ld b, 0");
                _i("jr z, .l%d", false_label);
                _i("bit 7, a");
                _i("jr %s, .l%d", nflag, false_label);
                _i("inc b");
                _label(false_label);
                _i("ld a, b");
            }
            return (Value) { .typeId = BOOL, .storage = ST_REG_VAL };
        default:
            printf("%d\n", op);
            assert(false);
            break;
    }
    return (Value) { .typeId = v1.typeId, .storage = ST_REG_VAL };
}

Value emit_unary_op_16(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v = emit_expression(n->expr.builtin.arg1, *frame);
    assert(v.typeId == U16 || v.typeId == I16);

    switch (op) {
        case OP_UNARY_NEG_16:
            v = emit_value_to_register(v, false); // v in `hl`
            _i("xor a");
            _i("sub a, l");
            _i("ld l, a");
            _i("xor a");
            _i("sbc a, h");
            _i("ld h, a");
            return (Value) { .typeId = v.typeId, .storage = ST_REG_VAL };
        case OP_NOT_16:
            v = emit_value_to_register(v, false); // v in `hl`
            _i("ld a, h");
            _i("cpl");
            _i("ld h, a");
            _i("ld a, l");
            _i("cpl");
            _i("ld l, a");
            return (Value) { .typeId = v.typeId, .storage = ST_REG_VAL };
        default:
            assert(false);
    }
}

Value emit_assign_sized(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);
    assert(get_type(v2.typeId)->type == TT_ARRAY ||
           get_type(v2.typeId)->type == TT_STRUCT);

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

Value emit_eq_sized(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);
    assert(get_type(v2.typeId)->type == TT_ARRAY ||
           get_type(v2.typeId)->type == TT_STRUCT);

    // v2 (src) in `de`
    _i("ld d, h");
    _i("ld e, l");
    v1 = emit_pop_temporary(v1, frame, false); // v1 (dest) in `hl`

    _i("ld bc, %d", get_type(v1.typeId)->size);
    _i("call __memcmp");

    return (Value) { .typeId = BOOL, .storage = ST_REG_VAL };
}

Value emit_assign_u16(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);

    v2 = emit_value_to_register(v2, true);   // v2 in `de`
    v1 = emit_pop_temporary(v1, frame, false);

    if (v1.storage != ST_REG_EA) {
        fatal_error(n->start_token, "Can not assign to temporary");
    }

    switch (op) {
        case OP_ASSIGN_16:
            _i("ld a, e");
            _i("ld [hl+], a");
            _i("ld a, d");
            _i("ld [hl-], a");
            break;
        case OP_ADD_ASSIGN_16:
            _i("ld a, [hl]");
            _i("add a, e");
            _i("ld [hl+], a");

            _i("ld a, [hl]");
            _i("adc a, d");
            _i("ld [hl-], a");
            break;
        case OP_SUB_ASSIGN_16:
            _i("ld a, [hl]");
            _i("sub a, e");
            _i("ld [hl+], a");

            _i("ld a, [hl]");
            _i("sbc a, d");
            _i("ld [hl-], a");
            break;
        case OP_OR_ASSIGN_16:
            _i("ld a, [hl]");
            _i("or a, e");
            _i("ld [hl+], a");

            _i("ld a, [hl]");
            _i("or a, d");
            _i("ld [hl-], a");
            break;
        case OP_AND_ASSIGN_16:
            _i("ld a, [hl]");
            _i("and a, e");
            _i("ld [hl+], a");

            _i("ld a, [hl]");
            _i("and a, d");
            _i("ld [hl-], a");
            break;
        case OP_XOR_ASSIGN_16:
            _i("ld a, [hl]");
            _i("xor a, e");
            _i("ld [hl+], a");

            _i("ld a, [hl]");
            _i("xor a, d");
            _i("ld [hl-], a");
            break;
        case OP_LSR_ASSIGN_16:
        case OP_LSL_ASSIGN_16:
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
                if (op == OP_LSL_ASSIGN_16) {
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
        case OP_MUL_ASSIGN_16:
            _i("push hl");
            emit_value_to_register(v1, false);
            _i("call __mul16");
            _i("ld d, h");
            _i("ld e, l");
            _i("pop hl");
            _i("ld [hl], e");
            _i("inc hl");
            _i("ld [hl], d");
            _i("dec hl");
            break;
        case OP_DIV_ASSIGN_U16:
        case OP_DIV_ASSIGN_I16:
            _i("push hl");
            emit_value_to_register(v1, false);
            if (op == OP_DIV_ASSIGN_U16) {
                _i("call __divu16");
            } else {
                _i("call __divi16");
            }
            _i("ld d, h");
            _i("ld e, l");
            _i("pop hl");
            _i("ld [hl], e");
            _i("inc hl");
            _i("ld [hl], d");
            _i("dec hl");
            break;
        case OP_MOD_ASSIGN_U16:
        case OP_MOD_ASSIGN_I16:
            _i("push hl");
            emit_value_to_register(v1, false);
            if (op == OP_MOD_ASSIGN_U16) {
                _i("call __divu16");
            } else {
                _i("call __divi16");
            }
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

Value emit_ptr_opassign_u16(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);

    if (v1.storage != ST_REG_EA) {
        fatal_error(n->start_token, "Can not assign to temporary");
    }

    assert(get_type(v1.typeId)->type == TT_PTR);
    const Type *ref = get_type(get_type(v1.typeId)->ptr.ref);

    v2 = emit_value_to_register(v2, true);   // v2 in `de`
    if (ref->size != 1) {
        _i("ld hl, %d", ref->size);
        _i("call __mul16");
        _i("ld d, h");
        _i("ld e, l");
    }
    v1 = emit_pop_temporary(v1, frame, false);
    if (op == OP_PTR_ADD_ASSIGN) {
        _i("ld a, [hl]");
        _i("add a, e");
        _i("ld [hl+], a");

        _i("ld a, [hl]");
        _i("adc a, d");
        _i("ld [hl-], a");
    } else if (op == OP_PTR_SUB_ASSIGN) {
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

Value emit_binop_16(AstNode *n, StackFrame *frame, enum BuiltinOp op, AstNode *expr1, AstNode *expr2) {
    Value v1 = emit_expression(n->expr.builtin.arg1, *frame);
    emit_push_temporary(n->expr.builtin.arg1, v1, frame);
    Value v2 = emit_expression(n->expr.builtin.arg2, *frame);

    assert(v1.typeId == U16 || v1.typeId == I16);

    v2 = emit_value_to_register(v2, true);   // v2 in `de`
    v1 = emit_pop_temporary(v1, frame, false);
    v1 = emit_value_to_register(v1, false);  // v1 in `hl`

    switch (op) {
        case OP_ASR_16:
        case OP_LSR_16:
        case OP_LSL_16:
            {
                const int start_label = _local_label_seq++;
                const int end_label = _local_label_seq++;
                // ignore high byte of shift value
                _i("ld a, e");
                _label(start_label);
                _i("and a");
                _i("jr z, .l%d", end_label);
                _i("dec a");
                if (op == OP_LSL_16) {
                    _i("sla l");
                    _i("rl h");
                } else if (op == OP_LSR_16) {
                    _i("srl h");
                    _i("rr l");
                } else if (op == OP_ASR_16) {
                    _i("sra h");
                    _i("rr l");
                } else assert(false);
                _i("jr .l%d", start_label);
                _label(end_label);
            }
            break;
        case OP_ADD_16:
            _i("add hl, de");
            break;
        case OP_SUB_16:
            _i("ld a, l");
            _i("sub a, e");
            _i("ld l, a");

            _i("ld a, h");
            _i("sbc a, d");
            _i("ld h, a");
            break;
        case OP_OR_16:
            _i("ld a, h");
            _i("or a, d");
            _i("ld h, a");

            _i("ld a, l");
            _i("or a, e");
            _i("ld l, a");
            break;
        case OP_AND_16:
            _i("ld a, h");
            _i("and a, d");
            _i("ld h, a");

            _i("ld a, l");
            _i("and a, e");
            _i("ld l, a");
            break;
        case OP_XOR_16:
            _i("ld a, h");
            _i("xor a, d");
            _i("ld h, a");

            _i("ld a, l");
            _i("xor a, e");
            _i("ld l, a");
            break;
        case OP_MUL_16:
            _i("call __mul16");
            break;
        case OP_DIV_U16:
            _i("call __divu16");
            break;
        case OP_DIV_I16:
            _i("call __divi16");
            break;
        case OP_MOD_I16:
            _i("call __divi16");
            _i("ld h, d");
            _i("ld l, e");
            break;
        case OP_MOD_U16:
            _i("call __divu16");
            _i("ld h, d");
            _i("ld l, e");
            break;
        case OP_NEQ_16:
        case OP_EQ_16:
            {
                const int l = _local_label_seq++;
                _i("ld a, l");
                _i("sub a, e");
                _i("ld l, a");

                _i("ld a, h");
                _i("sbc a, d");
                _i("or a, l");

                _i("ld a, 0"); // not affecting flags

                _i("jr %s, .l%d", op == OP_EQ_16 ? "nz" : "z", l);
                _i("dec a");
                _label(l);
            }
            return (Value) { .typeId = BOOL, .storage = ST_REG_VAL };
        case OP_LTE_I16:
        case OP_GT_I16:
            {
                const int end_label = _local_label_seq++;
                const char *nflag = op == OP_GT_I16 ? "z" : "nz";
                _i("ld a, e");
                _i("sub a, l");
                _i("ld e, a");
                _i("ld a, d");
                _i("sbc a, h");
                _i("ld d, a");

                _i("bit 7, d");
                _i("ld a, 0");
                _i("jr %s, .l%d", nflag, end_label);
                _i("inc a");
                _label(end_label);
            }
            return (Value) { .typeId = BOOL, .storage = ST_REG_VAL };
        case OP_GTE_I16:
        case OP_LT_I16:
            {
                const int end_label = _local_label_seq++;
                const char *nflag = op == OP_LT_I16 ? "z" : "nz";
                _i("ld a, l");
                _i("sub a, e");
                _i("ld l, a");
                _i("ld a, h");
                _i("sbc a, d");
                _i("ld h, a");

                _i("bit 7, h");
                _i("ld a, 0");
                _i("jr %s, .l%d", nflag, end_label);
                _i("inc a");
                _label(end_label);
            }
            return (Value) { .typeId = BOOL, .storage = ST_REG_VAL };
        case OP_GT_U16:
        case OP_LT_U16:
            {
                const int test_lo_label = _local_label_seq++;
                const int end_label = _local_label_seq++;
                const char *cflag = op == OP_GT_U16 ? "c" : "nc";
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
            return (Value) { .typeId = BOOL, .storage = ST_REG_VAL };
        case OP_GTE_U16:
        case OP_LTE_U16:
            {
                const int test_lo_label = _local_label_seq++;
                const int end_label = _local_label_seq++;
                const char *cflag = op == OP_GTE_U16 ? "nc" : "c";
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
            return (Value) { .typeId = BOOL, .storage = ST_REG_VAL };
        default:
            assert(false);
            break;
    }
    return (Value) { .typeId = v1.typeId, .storage = ST_REG_VAL };
}

static Value emit_builtin(AstNode *n, StackFrame frame) {
    assert(n->type == AST_EXPR && n->expr.type == EXPR_BUILTIN);
    enum BuiltinOp op = n->expr.builtin.op;

    Value result;

    switch (op) {
        case OP_DIV_I8:
        case OP_MOD_I8:
        case OP_ADD_8:
        case OP_SUB_8:
        case OP_AND_8:
        case OP_OR_8:
        case OP_XOR_8:
        case OP_MUL_8:
        case OP_DIV_U8:
        case OP_MOD_U8:
        case OP_LSL_8:
        case OP_LSR_8:
        case OP_ASR_8:
        case OP_EQ_8:
        case OP_NEQ_8:
        case OP_LT_U8:
        case OP_GT_U8:
        case OP_LTE_U8:
        case OP_GTE_U8:
        case OP_LT_I8:
        case OP_GT_I8:
        case OP_LTE_I8:
        case OP_GTE_I8:
            result = emit_binop_u8(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_ASR_16:
        case OP_ADD_16:
        case OP_SUB_16:
        case OP_AND_16:
        case OP_OR_16:
        case OP_XOR_16:
        case OP_MUL_16:
        case OP_DIV_U16:
        case OP_MOD_U16:
        case OP_DIV_I16:
        case OP_MOD_I16:
        case OP_LSL_16:
        case OP_LSR_16:
        case OP_EQ_16:
        case OP_NEQ_16:
        case OP_LT_U16:
        case OP_GT_U16:
        case OP_LTE_U16:
        case OP_GTE_U16:
        case OP_LT_I16:
        case OP_GT_I16:
        case OP_LTE_I16:
        case OP_GTE_I16:
            result = emit_binop_16(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_NOT_8:
        case OP_UNARY_NEG_8:
            result = emit_unary_op_8(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_NOT_16:
        case OP_UNARY_NEG_16:
            result = emit_unary_op_16(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_LOGICAL_AND:
            result = emit_logical_and(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_LOGICAL_OR:
            result = emit_logical_or(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_LOGICAL_NOT:
            result = emit_logical_not(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_ASSIGN_8:
        case OP_ADD_ASSIGN_8:
        case OP_SUB_ASSIGN_8:
        case OP_MUL_ASSIGN_8:
        case OP_DIV_ASSIGN_U8:
        case OP_MOD_ASSIGN_U8:
        case OP_DIV_ASSIGN_I8:
        case OP_MOD_ASSIGN_I8:
        case OP_LSL_ASSIGN_8:
        case OP_LSR_ASSIGN_8:
        case OP_ASR_ASSIGN_8:
        case OP_AND_ASSIGN_8:
        case OP_OR_ASSIGN_8:
        case OP_XOR_ASSIGN_8:
            result = emit_assign_8(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_ASSIGN_16:
        case OP_ADD_ASSIGN_16:
        case OP_SUB_ASSIGN_16:
        case OP_MUL_ASSIGN_16:
        case OP_DIV_ASSIGN_U16:
        case OP_MOD_ASSIGN_U16:
        case OP_DIV_ASSIGN_I16:
        case OP_MOD_ASSIGN_I16:
        case OP_LSL_ASSIGN_16:
        case OP_LSR_ASSIGN_16:
        case OP_ASR_ASSIGN_16:
        case OP_AND_ASSIGN_16:
        case OP_OR_ASSIGN_16:
        case OP_XOR_ASSIGN_16:
            result = emit_assign_u16(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_ASSIGN_SIZED: /* structs, arrays, etc */
            result = emit_assign_sized(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_NEQ_SIZED: /* structs, arrays, etc */
            result = emit_eq_sized(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            _i("xor a, 1"); // not
            break;
        case OP_EQ_SIZED: /* structs, arrays, etc */
            result = emit_eq_sized(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_ARRAY_INDEX_8:
            result = emit_array_indexing_u8(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_ARRAY_INDEX_16:
            result = emit_array_indexing_u16(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_ADDRESSOF:
            result = emit_addressof(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_DEREF:
            result = emit_ptr_deref(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_PTR_ADD:
        case OP_PTR_SUB:
            result = emit_ptr_addsub_u16(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
        case OP_PTR_ADD_ASSIGN:
        case OP_PTR_SUB_ASSIGN:
            result = emit_ptr_opassign_u16(n, &frame, op, n->expr.builtin.arg1, n->expr.builtin.arg2);
            break;
    }
    if (!is_type_eq(result.typeId, n->expr.eval_type)) {
        fatal_error(n->start_token, "Compiler bug. Operator operator yielded the wrong type. Expected %.*s but got %.*s",
                (int)get_type(n->expr.eval_type)->name.len,
                get_type(n->expr.eval_type)->name.s,
                (int)get_type(result.typeId)->name.len,
                get_type(result.typeId)->name.s);
    }
    return result;
}

static Value emit_zero_extend_8_16(Value v, TypeId to_type) {
    assert(v.typeId == U8 || v.typeId == I8);
    assert(to_type == U16 || to_type == I16);

    emit_value_to_register(v, false);  // v1 in `a`
    _i("ld h, 0");
    _i("ld l, a");
    return (Value) { .typeId = to_type, .storage = ST_REG_VAL };
}

static Value emit_sign_extend_i8_i16(Value v) {
    assert(v.typeId == I8);
    emit_value_to_register(v, false);  // v in `a`
    const int on_pve = _local_label_seq++;
    _i("ld l, a");
    _i("ld h, 0");
    _i("bit 7, a");
    _i("jr z, .l%d", on_pve);
    _i("dec h");
    _label(on_pve);
    return (Value) { .typeId = I16, .storage = ST_REG_VAL };
}

static Value emit_truncate_16_8(Value v, TypeId to_type) {
    assert(to_type == U8 || to_type == I8);

    emit_value_to_register(v, false);  // v in `hl`
    _i("ld a, l");
    return (Value) { .typeId = to_type, .storage = ST_REG_VAL };
}

static Value emit_cast(AstNode *n, StackFrame frame) {
    assert(n->type == AST_EXPR && n->expr.type == EXPR_CAST);

    TypeId to_type = n->expr.cast.to_type;
    Value v1 = emit_expression(n->expr.cast.arg, frame);

    /* Miscellaneous li type casts */
    if ((v1.typeId == to_type) ||
        (get_type(to_type)->type == TT_PTR && get_type(v1.typeId)->type == TT_PTR) ||
        (v1.typeId == U16 && get_type(to_type)->type == TT_PTR) ||
        (to_type == U16 && get_type(v1.typeId)->type == TT_PTR)) {
        goto no_op_cast;
    } else if (get_type(v1.typeId)->type == TT_ARRAY &&
               get_type(to_type)->type == TT_PTR &&
               is_type_eq(get_type(v1.typeId)->array.contained,
                          get_type(to_type)->ptr.ref)) {
        // consider EA of array to be REG_VAL of ptr
        assert(v1.storage == ST_REG_EA);
        return (Value) { .typeId = to_type, .storage = ST_REG_VAL };
    }
 
    /* all those godddddddam integer to integer conversions */
    switch (v1.typeId) {
        case U8: {
                switch (to_type) {
                    case U8: goto no_op_cast;
                    case I8: goto no_op_cast;
                    case U16:
                    case I16:
                        return emit_zero_extend_8_16(v1, to_type);
                    default: assert(false);
                }
            }
            break;
        case I8: {
                switch (to_type) {
                    case U8: goto no_op_cast;
                    case I8: goto no_op_cast;
                    case U16:
                        return emit_zero_extend_8_16(v1, to_type);
                    case I16:
                        return emit_sign_extend_i8_i16(v1);
                    default: assert(false);
                }
            }
            break;
        case U16: {
                switch (to_type) {
                    case U8:
                    case I8:
                        return emit_truncate_16_8(v1, to_type);
                    case U16: goto no_op_cast;
                    case I16: goto no_op_cast;
                    default: assert(false);
                }
            }
            break;
        case I16: {
                switch (to_type) {
                    case U8:
                    case I8:
                        return emit_truncate_16_8(v1, to_type);
                    case U16: goto no_op_cast;
                    case I16: goto no_op_cast;
                    default: assert(false);
                }
            }
            break;
        default: assert(false);
    }

    assert(false);

no_op_cast:
    return (Value) { .typeId = to_type, .storage = v1.storage };
}

static void emit_call_push_args(int arg_num, AstNode *arg_list_head, StackFrame *frame) {
    // push last to first
    if (arg_list_head == 0) {
        return;
    }

    if (arg_list_head->next_sibling != 0) {
        emit_call_push_args(arg_num+1, arg_list_head->next_sibling, frame);
    }

    assert(arg_list_head->type == AST_EXPR);
    Value v = emit_expression(arg_list_head, *frame);
    emit_push_fn_arg(arg_list_head, v, frame);
}

static Value emit_call(AstNode *n, StackFrame frame) {
    assert(n->type == AST_EXPR && n->expr.type == EXPR_CALL);

    // is it a built-in op?
    AstNode *callee = n->expr.call.callee;

    const int return_label = _local_label_seq++;
    // call by function pointer
    const int old_stack = frame.stack_offset;
    emit_call_push_args(0, n->expr.call.first_arg, &frame);

    if (n->expr.call.is_indirect) {
        // hl = function address 
        emit_expression(n->expr.call.callee, frame);
        // since there is no computed CALL opcode in LR35902, we explicitly
        // push the return address and jump to target
        _i("ld de, .l%d", return_label);
        _i("push de");
        _i("jp hl");
        _label(return_label);
    } else {
        assert(callee->expr.type == EXPR_IDENT);
        _i("call %.*s", (int)callee->expr.ident.len, callee->expr.ident.s);
    }
    const int stack_correction = frame.stack_offset - old_stack;
    if (stack_correction) {
        _i("add sp, %d", stack_correction);
    }
    frame.stack_offset += stack_correction;

    assert(get_type(callee->expr.eval_type)->type == TT_FUNC);
    TypeId ret = get_type(callee->expr.eval_type)->func.ret;

    return (Value) { .typeId = ret, .storage = get_type(ret)->type == TT_ARRAY ? ST_REG_EA : ST_REG_VAL };
}

static Value emit_loop(AstNode *n, StackFrame frame) {
    assert(n->type == AST_EXPR && n->expr.type == EXPR_LOOP);

    const int jump_label = _local_label_seq++;

    vec_push(&_jump_labels, &(AstNodeJumpLabel) {
        .node = n,
        .label_num = jump_label,
        .stack_offset = frame.stack_offset,
    });

    const AstNode *on_next_iter = n->expr.loop.on_next_iter;

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
        if (condition.typeId != BOOL) fatal_error(n->start_token, "FUCK");
        emit_bool_to_z_flag(condition);
        _i("jp z, .l%d_break", jump_label);
    }
    /*Value body =*/ emit_expression(n->expr.loop.body, frame);
    _i("jp .l%d_continue", jump_label);
    __(".l%d_break:", jump_label);

    return (Value) { .typeId = VOID, .storage = ST_REG_VAL };
}

static Value emit_if_else(AstNode *n, StackFrame frame) {
    assert(n->type == AST_EXPR && n->expr.type == EXPR_IF_ELSE);

    Value condition = emit_expression(n->expr.if_else.condition, frame);

    const int else_label = _local_label_seq++;
    const int end_label = _local_label_seq++;

    emit_bool_to_z_flag(condition);
    _i("jp z, .l%d", else_label);

    Value on_true = emit_expression(n->expr.if_else.on_true, frame);
    on_true = emit_value_to_register(on_true, false);

    if (n->expr.if_else.on_false != 0) {
        _i("jp .l%d", end_label);
        _label(else_label);

        Value on_false = emit_expression(n->expr.if_else.on_false, frame);
        on_false = emit_value_to_register(on_false, false);

        // assume on_true and on_false types matched (frontend checked this)

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

static Value emit_identifier(AstNode *n, StackFrame frame) {
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
        _i("ld hl, %.*s", (int)n->expr.ident.len, n->expr.ident.s);

        if (global->type == AST_DEF_VAR) {
            return (Value) { .typeId = global->var_def.type, .storage = ST_REG_EA };
        }
        else if (global->type == AST_FN) {
            return (Value) { .typeId = global->fn.type, .storage = ST_REG_EA };
        }
        else {
            assert(false);
        }
    }

    fatal_error(n->start_token, "Compiler bug. Undefined variable '%.*s' reached code generator", (int)n->expr.ident.len, n->expr.ident.s);
}

static Value emit_local_scope(AstNode *n, StackFrame frame) {
    assert(n->type == AST_EXPR && n->expr.type == EXPR_LOCAL_SCOPE);

    /*StackVarIdx var =*/ alloc_stack_var(n->start_token, frame, (StackVar) {
        .type = n->expr.local_scope.var_type,
        .ident = n->expr.local_scope.var_name,
        .offset = frame.locals_top,
        .is_fn_arg = false
    });
    frame.locals_top += get_type(n->expr.local_scope.var_type)->size;
    frame.num_vars++;
    Value v = emit_expression(n->expr.local_scope.scoped_expr, frame);
    unalloc_stack_var();
    return v;
}

static Value emit_return(AstNode *val, StackFrame frame) {
    Value ret_val = emit_expression(val, frame);
    emit_value_to_register(ret_val, false);

    if (frame.locals_size) {
        _i("add sp, %d", frame.locals_size);
    }

    _i("ret");
    return ret_val;
}

static Value emit_expression(AstNode *n, StackFrame frame) {
    Value v = (Value) { .typeId = VOID, .storage = ST_REG_VAL };
    assert(n->type == AST_EXPR);

    switch (n->expr.type) {
        case EXPR_GOTO:
            {
                AstNodeJumpLabel *l = lookup_jump_label(n->expr.goto_.target);
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
        case EXPR_FE_OPERATOR:
            // should not reach backend
            assert(false);
        case EXPR_BUILTIN:
            return emit_builtin(n, frame);
        case EXPR_ASM:
            fprintf(output, "\n%.*s\n", (int)n->expr.asm_.asm_text.len, n->expr.asm_.asm_text.s);
            return (Value) { .typeId = VOID, .storage = ST_REG_VAL };
        case EXPR_CALL:
            return emit_call(n, frame);
        case EXPR_LIST:
            for (AstNode *e=n->expr.list.first_child; e != 0; e=e->next_sibling) {
                v = emit_expression(e, frame);
            }
            break;
        case EXPR_IDENT:
            return emit_identifier(n, frame);
        case EXPR_LITERAL:
            switch (n->expr.literal.type) {
                case LIT_INT_ANY:
                    // should not reach backend
                    assert(false);
                case LIT_BOOL:
                    _i("ld a, %hhu", (int8_t)(n->expr.literal.literal_bool ? 1 : 0));
                    v = (Value) { .typeId = BOOL, .storage = ST_REG_VAL };
                    break;
                case LIT_U8:
                case LIT_I8:
                    _i("ld a, %hhu", (int8_t)n->expr.literal.literal_int);
                    v = (Value) { .typeId = n->expr.eval_type, .storage = ST_REG_VAL };
                    break;
                case LIT_U16:
                case LIT_I16:
                    _i("ld hl, %hu", (int16_t)n->expr.literal.literal_int);
                    v = (Value) { .typeId = n->expr.eval_type, .storage = ST_REG_VAL };
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
                            .value = n
                        });
                        _i("ld hl, __L%lx", (uint64_t)n);
                    }
                    v = (Value) { .typeId = n->expr.eval_type, .storage = ST_REG_EA };
                    break;
                case LIT_ARRAY:
                    assert(false);
                    break;
            }
            break;
        case EXPR_CAST:
            return emit_cast(n, frame);
        case EXPR_IF_ELSE:
            return emit_if_else(n, frame);
        case EXPR_LOOP:
            return emit_loop(n, frame);
        case EXPR_LOCAL_SCOPE:
            return emit_local_scope(n, frame);
        case EXPR_RETURN:
            return emit_return(n->expr.return_.val, frame);
        case EXPR_MEMBER_ACCESS:
            {
                v = emit_expression(n->expr.member_access.struct_expr, frame);
                assert(v.storage == ST_REG_EA);
                const StructMember *member = lookup_struct_member(v.typeId, n->expr.member_access.member);
                if (member->offset != 0) {
                    _i("ld de, %d", member->offset);
                    _i("add hl, de");
                }
                v = (Value) { .typeId = member->type, .storage = ST_REG_EA };
            }
            break;
        default:
            assert(false);
    }
    return v;
}

// just records for ASM emission after code has been emitted
static void record_def_var(AstNode *var_node) {
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

static int get_max_local_vars_size(AstNode *node)
{
    if (node == NULL) return 0;

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
            for (AstNode *c=node->expr.list.first_child; c!=0; c=c->next_sibling) {
                size = max(size, get_max_local_vars_size(c));
            }
            break;
        case EXPR_IDENT:
            break;
        case EXPR_LITERAL:
            break;
        case EXPR_MEMBER_ACCESS:
            size = max(size, get_max_local_vars_size(node->expr.member_access.struct_expr));
            break;
        case EXPR_CALL:
            for (AstNode *c=node->expr.call.first_arg; c!=0; c=c->next_sibling) {
                size = max(size, get_max_local_vars_size(c));
            }
            break;
        case EXPR_FE_OPERATOR:
            // frontend structure (_FE_) should not reach backend
            assert(false);
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

static Value emit_fn(AstNode *fn_node) {
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

    for (AstNode *arg=fn_node->fn.first_arg; arg != 0; arg=arg->next_sibling) {
        assert(arg->type == AST_FN_ARG);
        
        TypeId argtype = arg->fn_arg.type;

        /*StackVarIdx v =*/ alloc_stack_var(arg->start_token, frame, (StackVar) {
            .type = argtype,
            .ident = arg->fn_arg.name,
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

static void _emit_const(AstNode *n) {
    assert(n->type == AST_EXPR && n->expr.type == EXPR_LITERAL);

    switch (n->expr.literal.type) {
        case LIT_BOOL:
            _i("db %hhu", (int8_t)(n->expr.literal.literal_bool ? 1 : 0));
            break;
        case LIT_U8:
        case LIT_I8:
            _i("db %hhu", (int8_t)n->expr.literal.literal_int);
            break;
        case LIT_U16:
        case LIT_I16:
            _i("dw %hu", (int16_t)n->expr.literal.literal_int);
            break;
        case LIT_VOID:
            break;
        case LIT_STR:
            _i("db \"%.*s\", 0",
                    n->expr.literal.literal_str.len,
                    n->expr.literal.literal_str.s);
            break;
        case LIT_ARRAY:
            for (AstNode *child=n->expr.literal.literal_array_first_val; child!=0; child=child->next_sibling) {
                _emit_const(child);
            }
            break;
        case LIT_INT_ANY:
            assert(false);
    }
}

static void emit_rom_globals() {
    for (int i=0; i<_global_vars.len; ++i) {
        GlobalVariable v = *(GlobalVariable*) vec_get(&_global_vars, i);
        if (v.is_const) {
            if (v.symbol_name.s) {
                __("%.*s:", v.symbol_name.len, v.symbol_name.s);
            } else {
                __("__L%lx:", (uint64_t)v.value /* is AstNode **/);
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
    _jump_labels = vec_init(sizeof(AstNodeJumpLabel));
}

void output_lr35902(Program *prog) {
    init();

    program = prog;
    output = fopen("out.asm", "w");

    AstNode *root_node = prog->root;
    assert(root_node->type == AST_MODULE);

    emit_boilerplate();

    for (AstNode *n=root_node->module.first_child; n != 0; n=n->next_sibling) {
        if (n->type == AST_FN) {
            if (n->fn.body != 0) {
                vec_zero(&_jump_labels);
                emit_fn(n);
            }
        }
        else if (n->type == AST_DEF_VAR) {
            record_def_var(n);
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
