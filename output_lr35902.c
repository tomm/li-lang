#include "output_lr35902.h"
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include "str.h"
#include "vec.h"
#include "tokenizer.h"
#include "types.h"

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

typedef struct Value {
    TypeId typeId;
    enum Storage {
        ST_REG_EA,
        ST_REG_VAL
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

static StackVar *get_stack_var(StackVarIdx idx) {
    return vec_get(&_stack_vars, idx);
}

static void emit_push(FILE *out, TypeId t, StackFrame *frame) {
    switch (t) {
        case VOID: abort();
        case U8:
            fprintf(out, "\t\tpush af\n");
            frame->stack_offset -= 2;
            return;
        default: abort();
    }

}

static void emit_pop(FILE *out, TypeId t, StackFrame *frame) {
    switch (t) {
        case VOID: abort();
        case U8:
            fprintf(out, "\t\tpop bc\n");
            frame->stack_offset += 2;
            return;
        default: abort();
    }

}
static TypeId emit_expression(FILE *out, NodeIdx expr, StackFrame frame);

static TypeId emit_builtin(FILE *out, NodeIdx call, StackFrame frame) {
    AstNode *n = get_node(call);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_BUILTIN);

    AstNode *arg1 = get_node(n->expr.builtin.first_arg);
    assert(arg1->next_sibling != 0);

    TypeId ta1 = emit_expression(out, arg1->next_sibling, frame);
    emit_push(out, ta1, &frame);
    TypeId ta2 = emit_expression(out, n->expr.builtin.first_arg, frame);

    assert(ta1 == U8 && ta2 == U8);
    emit_pop(out, ta1, &frame);

    // a=arg1, b=arg2

    // assumes binary op
    switch (n->expr.builtin.op) {
        case BUILTIN_ADD:
            fprintf(out, "\t\tadd a, b\n");
            break;
        case BUILTIN_SUB:
            fprintf(out, "\t\tsub a, b\n");
            break;
        case BUILTIN_BITOR:
            fprintf(out, "\t\tor a, b\n");
            break;
        case BUILTIN_BITAND:
            fprintf(out, "\t\tand a, b\n");
            break;
        case BUILTIN_BITXOR:
            fprintf(out, "\t\txor a, b\n");
            break;
        default:
            compile_error(n, "builtin operator %d not implemented", n->expr.builtin.op);
            abort();
            break;
    }
    return U8;
}

static TypeId emit_call(FILE *out, NodeIdx call, StackFrame frame) {
    TypeId t = VOID;
    AstNode *n = get_node(call);
    assert(n->type == AST_EXPR && n->expr.type == EXPR_CALL);

    // is it a built-in op?
    AstNode *callee = get_node(n->expr.call.callee);
    if (callee->type == AST_EXPR && callee->expr.type == EXPR_IDENT) {
        compile_error(callee, "fn call by identifier not implemented");
    } else {
        compile_error(callee, "fn call by expression not implemented");
    }

    return t;
}

static TypeId emit_expression(FILE *out, NodeIdx expr, StackFrame frame) {
    TypeId t = VOID;
    AstNode *n = get_node(expr);
    assert(n->type == AST_EXPR);

    switch (n->expr.type) {
        case EXPR_BUILTIN:
            return emit_builtin(out, expr, frame);
        case EXPR_CALL:
            return emit_call(out, expr, frame);
        case EXPR_LIST:
            for (NodeIdx e=n->expr.list.first_child; e != 0; e=get_node(e)->next_sibling) {
                t = emit_expression(out, e, frame);
            }
            break;
        case EXPR_IDENT:
            abort();
            break;
        case EXPR_LITERAL:
            fprintf(out, "\t\tld a, %.*s\n", (int)n->expr.literal.len, n->expr.literal.s);
            t = U8;
            break;
    }
    return t;
}

static TypeId find_type(AstNode *n, Str typename) {
    printf("Looking up type %.*s\n", (int)typename.len, typename.s);
    TypeId id = lookup_type(typename);
    if (id == -1) {
        compile_error(n, "Unknown type %.*s", (int)typename.len, typename.s);
    }
    return id;
}

static void emit_fn(FILE *out, NodeIdx fn) {
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
    
    TypeId ret_type = emit_expression(out, fn_node->fn.body, frame);
    assert(ret_type == U8);

    // expect U8 result in 'a' register
    fprintf(out, "\t\tret\n");
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
