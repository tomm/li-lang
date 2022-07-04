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
    AstNode *node;
} JumpLabel;

typedef struct Scope {
    Vec /*<Variable>*/ vars;
    Vec /*<JumpLabel>*/ labels;
    TypeId return_type;
} Scope;
static Scope new_localscope(TypeId return_type) {
    return (Scope){
        .vars = vec_init(sizeof(Variable)),
        .labels = vec_init(sizeof(JumpLabel)),
        .return_type = return_type
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

static TypeId typecheck_expr(Program *prog, Scope *scope, AstNode *expr, TypeId type_hint);

typedef struct ValidOperator {
    enum OperatorOp operator;
    enum BuiltinOp op;
    enum TypeType arg1;
    enum TypeType arg2;
    TypeId ret;
} ValidOperator;

static const ValidOperator valid_operators[] = {
    // pointer stuff
    { OPERATOR_UNARY_ADDRESSOF, OP_ADDRESSOF, -1 /* any */, TT_PRIM_VOID, -1 },
    { OPERATOR_UNARY_DEREF, OP_DEREF, TT_PTR, TT_PRIM_VOID, -1 /* any */ },
    { OPERATOR_ASSIGN, OP_ASSIGN_16, TT_PTR, TT_PTR, -1 },
    { OPERATOR_ASSIGN, OP_ASSIGN_SIZED, TT_ARRAY, TT_ARRAY, -1 },
    { OPERATOR_ASSIGN, OP_ASSIGN_SIZED, TT_STRUCT, TT_STRUCT, -1 },
    { OPERATOR_EQ, OP_EQ_SIZED, TT_ARRAY, TT_ARRAY, BOOL },
    { OPERATOR_EQ, OP_EQ_SIZED, TT_STRUCT, TT_STRUCT, BOOL },
    { OPERATOR_NEQ, OP_NEQ_SIZED, TT_ARRAY, TT_ARRAY, BOOL },
    { OPERATOR_NEQ, OP_NEQ_SIZED, TT_STRUCT, TT_STRUCT, BOOL },
    { OPERATOR_ARRAY_INDEXING, OP_ARRAY_INDEX_8, TT_ARRAY, TT_PRIM_U8, -1 /* any */ },
    { OPERATOR_ARRAY_INDEXING, OP_ARRAY_INDEX_16, TT_ARRAY, TT_PRIM_U16, -1 /* any */ },
    { OPERATOR_ADD, OP_PTR_ADD, TT_PTR, TT_PRIM_U16, -1 },
    { OPERATOR_SUB, OP_PTR_SUB, TT_PTR, TT_PRIM_U16, -1 },
    { OPERATOR_PLUSASSIGN, OP_PTR_ADD_ASSIGN, TT_PTR, TT_PRIM_U16, -1 },
    { OPERATOR_MINUSASSIGN, OP_PTR_SUB_ASSIGN, TT_PTR, TT_PRIM_U16, -1 },
    // logical
    { OPERATOR_UNARY_LOGICAL_NOT, OP_LOGICAL_NOT, TT_PRIM_BOOL, TT_PRIM_VOID, BOOL },
    { OPERATOR_LOGICAL_AND, OP_LOGICAL_AND, TT_PRIM_BOOL, TT_PRIM_BOOL, BOOL },
    { OPERATOR_LOGICAL_OR, OP_LOGICAL_OR, TT_PRIM_BOOL, TT_PRIM_BOOL, BOOL },
    // u8
    { OPERATOR_UNARY_NEG, OP_UNARY_NEG_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_SHIFT_LEFT, OP_LSL_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_SHIFT_RIGHT, OP_LSR_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_ADD, OP_ADD_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_SUB, OP_SUB_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_NEQ, OP_NEQ_8, TT_PRIM_U8, TT_PRIM_U8, BOOL },
    { OPERATOR_EQ, OP_EQ_8, TT_PRIM_U8, TT_PRIM_U8, BOOL },
    { OPERATOR_GT, OP_GT_U8, TT_PRIM_U8, TT_PRIM_U8, BOOL },
    { OPERATOR_LT, OP_LT_U8, TT_PRIM_U8, TT_PRIM_U8, BOOL },
    { OPERATOR_GTE, OP_GTE_U8, TT_PRIM_U8, TT_PRIM_U8, BOOL },
    { OPERATOR_LTE, OP_LTE_U8, TT_PRIM_U8, TT_PRIM_U8, BOOL },
    { OPERATOR_MUL, OP_MUL_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_DIV, OP_DIV_U8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_MODULO, OP_MOD_U8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_BITXOR, OP_XOR_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_BITAND, OP_AND_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_BITOR, OP_OR_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_PLUSASSIGN, OP_ADD_ASSIGN_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_MINUSASSIGN, OP_SUB_ASSIGN_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_MULASSIGN, OP_MUL_ASSIGN_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_DIVASSIGN, OP_DIV_ASSIGN_U8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_MODASSIGN, OP_MOD_ASSIGN_U8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_LSHIFTASSIGN, OP_LSL_ASSIGN_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_RSHIFTASSIGN, OP_LSR_ASSIGN_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_BITANDASSIGN, OP_AND_ASSIGN_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_BITORASSIGN, OP_OR_ASSIGN_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_BITXORASSIGN, OP_XOR_ASSIGN_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_ASSIGN, OP_ASSIGN_8, TT_PRIM_U8, TT_PRIM_U8, U8 },
    { OPERATOR_UNARY_NEG, OP_UNARY_NEG_8, TT_PRIM_U8, TT_PRIM_VOID, U8 },
    { OPERATOR_UNARY_BITNOT, OP_NOT_8, TT_PRIM_U8, TT_PRIM_VOID, U8 },
    // i8
    { OPERATOR_UNARY_NEG, OP_UNARY_NEG_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_SHIFT_LEFT, OP_LSL_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_SHIFT_RIGHT, OP_ASR_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_ADD, OP_ADD_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_SUB, OP_SUB_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_NEQ, OP_NEQ_8, TT_PRIM_I8, TT_PRIM_I8, BOOL },
    { OPERATOR_EQ, OP_EQ_8, TT_PRIM_I8, TT_PRIM_I8, BOOL },
    { OPERATOR_GT, OP_GT_I8, TT_PRIM_I8, TT_PRIM_I8, BOOL },
    { OPERATOR_LT, OP_LT_I8, TT_PRIM_I8, TT_PRIM_I8, BOOL },
    { OPERATOR_GTE, OP_GTE_I8, TT_PRIM_I8, TT_PRIM_I8, BOOL },
    { OPERATOR_LTE, OP_LTE_I8, TT_PRIM_I8, TT_PRIM_I8, BOOL },
    { OPERATOR_MUL, OP_MUL_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_DIV, OP_DIV_I8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_MODULO, OP_MOD_I8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_BITXOR, OP_XOR_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_BITAND, OP_AND_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_BITOR, OP_OR_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_PLUSASSIGN, OP_ADD_ASSIGN_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_MINUSASSIGN, OP_SUB_ASSIGN_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_MULASSIGN, OP_MUL_ASSIGN_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_DIVASSIGN, OP_DIV_ASSIGN_I8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_MODASSIGN, OP_MOD_ASSIGN_I8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_LSHIFTASSIGN, OP_LSL_ASSIGN_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_RSHIFTASSIGN, OP_ASR_ASSIGN_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_BITANDASSIGN, OP_AND_ASSIGN_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_BITORASSIGN, OP_OR_ASSIGN_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_BITXORASSIGN, OP_XOR_ASSIGN_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_ASSIGN, OP_ASSIGN_8, TT_PRIM_I8, TT_PRIM_I8, I8 },
    { OPERATOR_UNARY_NEG, OP_UNARY_NEG_8, TT_PRIM_I8, TT_PRIM_VOID, I8 },
    { OPERATOR_UNARY_BITNOT, OP_NOT_8, TT_PRIM_I8, TT_PRIM_VOID, I8 },
    // u16
    { OPERATOR_UNARY_NEG, OP_UNARY_NEG_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_SHIFT_LEFT, OP_LSL_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_SHIFT_RIGHT, OP_LSR_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_ADD, OP_ADD_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_SUB, OP_SUB_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_NEQ, OP_NEQ_16, TT_PRIM_U16, TT_PRIM_U16, BOOL },
    { OPERATOR_EQ, OP_EQ_16, TT_PRIM_U16, TT_PRIM_U16, BOOL },
    { OPERATOR_GT, OP_GT_U16, TT_PRIM_U16, TT_PRIM_U16, BOOL },
    { OPERATOR_LT, OP_LT_U16, TT_PRIM_U16, TT_PRIM_U16, BOOL },
    { OPERATOR_GTE, OP_GTE_U16, TT_PRIM_U16, TT_PRIM_U16, BOOL },
    { OPERATOR_LTE, OP_LTE_U16, TT_PRIM_U16, TT_PRIM_U16, BOOL },
    { OPERATOR_MUL, OP_MUL_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_DIV, OP_DIV_U16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_MODULO, OP_MOD_U16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_BITXOR, OP_XOR_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_BITAND, OP_AND_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_BITOR, OP_OR_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_PLUSASSIGN, OP_ADD_ASSIGN_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_MINUSASSIGN, OP_SUB_ASSIGN_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_MULASSIGN, OP_MUL_ASSIGN_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_DIVASSIGN, OP_DIV_ASSIGN_U16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_MODASSIGN, OP_MOD_ASSIGN_U16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_LSHIFTASSIGN, OP_LSL_ASSIGN_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_RSHIFTASSIGN, OP_LSR_ASSIGN_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_BITANDASSIGN, OP_AND_ASSIGN_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_BITORASSIGN, OP_OR_ASSIGN_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_BITXORASSIGN, OP_XOR_ASSIGN_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_ASSIGN, OP_ASSIGN_16, TT_PRIM_U16, TT_PRIM_U16, U16 },
    { OPERATOR_UNARY_NEG, OP_UNARY_NEG_16, TT_PRIM_U16, TT_PRIM_VOID, U16 },
    { OPERATOR_UNARY_BITNOT, OP_NOT_16, TT_PRIM_U16, TT_PRIM_VOID, U16 },
    // i16
    { OPERATOR_UNARY_NEG, OP_UNARY_NEG_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_SHIFT_LEFT, OP_LSL_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_SHIFT_RIGHT, OP_ASR_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_ADD, OP_ADD_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_SUB, OP_SUB_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_NEQ, OP_NEQ_16, TT_PRIM_I16, TT_PRIM_I16, BOOL },
    { OPERATOR_EQ, OP_EQ_16, TT_PRIM_I16, TT_PRIM_I16, BOOL },
    { OPERATOR_GT, OP_GT_I16, TT_PRIM_I16, TT_PRIM_I16, BOOL },
    { OPERATOR_LT, OP_LT_I16, TT_PRIM_I16, TT_PRIM_I16, BOOL },
    { OPERATOR_GTE, OP_GTE_I16, TT_PRIM_I16, TT_PRIM_I16, BOOL },
    { OPERATOR_LTE, OP_LTE_I16, TT_PRIM_I16, TT_PRIM_I16, BOOL },
    { OPERATOR_MUL, OP_MUL_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_DIV, OP_DIV_I16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_MODULO, OP_MOD_I16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_BITXOR, OP_XOR_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_BITAND, OP_AND_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_BITOR, OP_OR_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_PLUSASSIGN, OP_ADD_ASSIGN_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_MINUSASSIGN, OP_SUB_ASSIGN_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_MULASSIGN, OP_MUL_ASSIGN_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_DIVASSIGN, OP_DIV_ASSIGN_I16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_MODASSIGN, OP_MOD_ASSIGN_I16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_LSHIFTASSIGN, OP_LSL_ASSIGN_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_RSHIFTASSIGN, OP_LSR_ASSIGN_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_BITANDASSIGN, OP_AND_ASSIGN_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_BITORASSIGN, OP_OR_ASSIGN_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_BITXORASSIGN, OP_XOR_ASSIGN_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_ASSIGN, OP_ASSIGN_16, TT_PRIM_I16, TT_PRIM_I16, I16 },
    { OPERATOR_UNARY_NEG, OP_UNARY_NEG_16, TT_PRIM_I16, TT_PRIM_VOID, I16 },
    { OPERATOR_UNARY_BITNOT, OP_NOT_16, TT_PRIM_I16, TT_PRIM_VOID, I16 },
    { -1 }
};

static bool try_fold_unary_neg(Program *prog, Scope *scope, AstNode *n, TypeId type_hint) {
    enum OperatorOp op = n->expr.fe_operator.op;
    assert(op == OPERATOR_UNARY_NEG);

    AstNode *a = n->expr.fe_operator.arg1;
    if (a->expr.type == EXPR_LITERAL &&
        (a->expr.literal.type == LIT_INT_ANY ||
         a->expr.literal.type == LIT_U8 ||
         a->expr.literal.type == LIT_I8 ||
         a->expr.literal.type == LIT_I16 ||
         a->expr.literal.type == LIT_U16)) {
        // turn OPERATOR_UNARY_NEG node into the literal node directly
        AstNode *next_sibling = n->next_sibling;
        *n = *a;
        n->next_sibling = next_sibling;
        n->expr.literal.literal_int = -n->expr.literal.literal_int;
        typecheck_expr(prog, scope, n, type_hint);
        return true;
    } else {
        return false;
    }
}

/* Pass -1 for TypeType to find any */
static const struct ValidOperator *find_first_matching_operator(enum OperatorOp op, enum TypeType t1, enum TypeType t2) {
    for (int i=0; valid_operators[i].operator != -1; ++i) {
        const struct ValidOperator *v = &valid_operators[i];
        if (v->operator == op && (v->arg1 == t1 || t1 == -1) && (v->arg2 == t2 || t2 == -1)) {
            return v;
        }
    }
    return NULL;
}

static TypeId int_typekind_to_typeid(enum TypeType typekind) {
    switch (typekind) {
        case TT_PRIM_U8: return U8;
        case TT_PRIM_I8: return I8;
        case TT_PRIM_U16: return U16;
        case TT_PRIM_I16: return I16;
        default: return TYPE_UNKNOWN;
    }
}

/* Transforms the untyped operator into a specific builtin op */
static TypeId typecheck_operator(Program *prog, Scope *scope, AstNode *n, TypeId expr_type_hint) {
    enum OperatorOp op = n->expr.fe_operator.op;

    const int num_args = n->expr.fe_operator.arg2 == 0 ? 1 : 2;

    AstNode *arg1 = n->expr.fe_operator.arg1;
    AstNode *arg2 = n->expr.fe_operator.arg2;

    /* Mandatory optimization... fold unary negative operator on constants -- needed for globals */
    if (op == OPERATOR_UNARY_NEG) {
        if (try_fold_unary_neg(prog, scope, n, expr_type_hint)) {
            return n->expr.eval_type;
        }
    }

    // All this hideous TT_UNKNOWN/TYPE_UNKNOWN stuff is due to size
    // inference of unsized integer literals...
    TypeId t1, t2;

    t1 = typecheck_expr(prog, scope, arg1, TYPE_UNKNOWN);

    // special type hint for assignment operator
    TypeId hint = op == OPERATOR_ASSIGN ? t1 : TYPE_UNKNOWN;
    t2 = num_args == 2 ? typecheck_expr(prog, scope, arg2, hint) : VOID;

    enum TypeType tt1 = get_type(t1)->type;
    enum TypeType tt2 = t2 ? get_type(t2)->type : TT_PRIM_VOID;

    if (t1 == TYPE_UNKNOWN && t2 == TYPE_UNKNOWN) {
        t1 = typecheck_expr(prog, scope, arg1, expr_type_hint);
        t2 = typecheck_expr(prog, scope, arg2, expr_type_hint);
    }
    else if (t1 == TYPE_UNKNOWN) {
        const struct ValidOperator *b = find_first_matching_operator(op, -1, tt2);
        TypeId hint = b ? int_typekind_to_typeid(b->arg1) : t2;
        t1 = typecheck_expr(prog, scope, arg1, hint);
    }
    else if (t2 == TYPE_UNKNOWN) {
        const struct ValidOperator *b = find_first_matching_operator(op, tt1, -1);
        TypeId hint = b ? int_typekind_to_typeid(b->arg2) : t1;
        t2 = typecheck_expr(prog, scope, arg2, hint);
    }

    tt1 = get_type(t1)->type;
    tt2 = t2 ? get_type(t2)->type : TT_PRIM_VOID;

    // do the argument types match a valid operator?
    for (int i=0; valid_operators[i].operator != -1; ++i) {
        const ValidOperator *v = &valid_operators[i];
        if (v->operator == op &&
            (v->arg1 == -1 || v->arg1 == tt1) &&
            (v->arg2 == -1 || v->arg2 == tt2)) {
            // convert fe_operator node to builtin node, that backend understands
            n->expr.type = EXPR_BUILTIN;
            n->expr.builtin.arg1 = arg1;
            n->expr.builtin.arg2 = arg2;
            n->expr.builtin.op = v->op;

            if (v->ret == -1) {
                // special handling of return type
                switch (op) {
                    case OPERATOR_UNARY_ADDRESSOF:
                        return make_ptr_type(t1);
                    case OPERATOR_UNARY_DEREF:
                        return get_type(t1)->ptr.ref;
                    case OPERATOR_ADD:
                    case OPERATOR_SUB:
                        if (tt1 == TT_PTR) return t1;
                        else assert(false);
                    case OPERATOR_EQ:
                    case OPERATOR_NEQ:
                    case OPERATOR_ASSIGN:
                        if (!is_type_eq(t1, t2)) {
                            goto error;
                        }
                        return t1;
                    case OPERATOR_MINUSASSIGN:
                        // ptr -= u16
                        return t1;
                    case OPERATOR_PLUSASSIGN:
                        // ptr += u16
                        return t1;
                    case OPERATOR_ARRAY_INDEXING:
                        return get_type(t1)->array.contained;
                    default: assert(false);
                }
            } else {
                return v->ret;
            }
        }
    }
error:
    fatal_error(n->start_token, "invalid arguments to operator '%s': '%.*s' and '%.*s'",
            operator_name(op),
            (int)get_type(t1)->name.len,
            get_type(t1)->name.s,
            (int)get_type(t2)->name.len,
            get_type(t2)->name.s);
}

static void check_int_literal_range(const Token *t, TypeId type, int val) {
    // XXX RE-enable this
    return;

    switch (type) {
        case U8:
            if (val < 0 || val > 255) {
                fatal_error(t, "U8 literal out of range [0..255]");
            }
            break;
        case I8:
            if (val < -128 || val > 127) {
                fatal_error(t, "I8 literal out of range [-128..127]");
            }
            break;
        case U16:
            if (val < 0 || val > 65535) {
                fatal_error(t, "U16 literal out of range [0..65535]");
            }
            break;
        case I16:
            if (val < -32768 || val > 32767) {
                fatal_error(t, "I16 literal out of range [-32768..32767]");
            }
            break;
    }
}

static void validate_return(const Token *t, TypeId expected, TypeId returned) {
    if (!is_type_eq(expected, returned)) {
        fatal_error(t, "function returned %.*s but should return %.*s",
                (int)get_type(returned)->name.len,
                get_type(returned)->name.s,
                (int)get_type(expected)->name.len,
                get_type(expected)->name.s);
    }
}

/**
 * const/var with certain types is forbidden: never, fn(...), void
 */
static void check_valid_binding_type(AstNode *n, TypeId t)
{
    if (t == TYPE_UNKNOWN || t == VOID || get_type(t)->type == TT_FUNC) {
        fatal_error(n->start_token, "'%.*s' is not a valid type for a var/const",
                get_type(t)->name.len,
                get_type(t)->name.s);
    }
}

static TypeId typecheck_call(Program *prog, Scope *scope, AstNode *call)
{
    assert(call->type == AST_EXPR && call->expr.type == EXPR_CALL);
    // is it a built-in op?
    AstNode *callee = call->expr.call.callee;
    TypeId fn_type = typecheck_expr(prog, scope, call->expr.call.callee, TYPE_UNKNOWN);

    // can assume a function call is static if the function name is just an identifier
    // (since for function pointers li requires at least a dereferencing operator as well)
    bool is_indirect = callee->expr.type != EXPR_IDENT;

    // update AST with 'is_indirect', now we know
    call->expr.call.is_indirect = is_indirect;

    if (get_type(fn_type)->type == TT_FUNC) {
        if (get_type(fn_type)->func.args.len !=
            ast_node_sibling_size(call->expr.call.first_arg)) {
            if (is_indirect) {
                fatal_error(callee->start_token, "function expected %d arguments but %d given",
                        (int)get_type(fn_type)->func.args.len,
                        ast_node_sibling_size(call->expr.call.first_arg));
            } else {
                fatal_error(callee->start_token, "function '%.*s' expected %d arguments but %d given",
                        (int)callee->expr.ident.len, callee->expr.ident.s,
                        (int)get_type(fn_type)->func.args.len,
                        ast_node_sibling_size(call->expr.call.first_arg));
            }
        }

        // check each argument
        Vec *argdef = &get_type(fn_type)->func.args;
        AstNode *arg = call->expr.call.first_arg;

        for (int i=0; i<argdef->len; ++i, arg = arg->next_sibling) {
            TypeId expected_type = *(TypeId*)vec_get(argdef, i);
            TypeId passed_type = typecheck_expr(prog, scope, arg, expected_type);
            if (!is_type_eq(passed_type, expected_type)) {

                fatal_error(arg->start_token, "error passing argument %d: type %.*s does not match expected type %.*s",
                        i + 1,
                        (int)get_type(passed_type)->name.len,
                        get_type(passed_type)->name.s,
                        (int)get_type(expected_type)->name.len,
                        get_type(expected_type)->name.s);
            }
        }

        return get_type(fn_type)->func.ret;
    } else {
        fatal_error(callee->start_token, "type '%.*s' is not a function",
                get_type(fn_type)->name.len,
                get_type(fn_type)->name.s);
    }
}

// XXX also resolves gotos. why do we do both? because AST isn't simple
// to traverse...
static TypeId typecheck_expr(Program *prog, Scope *scope, AstNode *n, TypeId type_hint) {
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
            t = NEVER;
            break;
        case EXPR_ASM:
            t = VOID;
            break;
        case EXPR_MEMBER_ACCESS:
            {
                TypeId struct_type = typecheck_expr(prog, scope, n->expr.member_access.struct_expr, type_hint);
                if (get_type(struct_type)->type != TT_STRUCT) {
                    fatal_error(n->start_token, "Member access on type that is not a struct or union");
                }
                const StructMember *mem = lookup_struct_member(struct_type, n->expr.member_access.member);
                if (mem == NULL) {
                    fatal_error(n->start_token, "Struct `%.*s` has no member `%.*s`",
                            get_type(struct_type)->name.len,
                            get_type(struct_type)->name.s,
                            n->expr.member_access.member.len,
                            n->expr.member_access.member.s);
                }
                t = mem->type;
            }
            break;
        case EXPR_CAST:
            {
                t = n->expr.cast.to_type;
                TypeId hint = get_type(t)->type == TT_PTR ? U16 : TYPE_UNKNOWN;
                TypeId from_type = typecheck_expr(prog, scope, n->expr.cast.arg, hint);

                // valid casts in li
                if (is_type_eq(from_type, t) ||
                    // casts between integers
                    (
                     (from_type == U8 || from_type == I8 || from_type == U16 || from_type == I16) &&
                     (t == U8 || t == I8 || t == U16 || t == I16)
                    ) ||
                    (get_type(t)->type == TT_PTR && from_type == U16) ||
                    (get_type(from_type)->type == TT_PTR && t == U16) ||
                    (get_type(from_type)->type == TT_PTR && get_type(t)->type == TT_PTR)
                ) {
                    // fine
                } else if (get_type(from_type)->type == TT_ARRAY &&
                           get_type(t)->type == TT_PTR &&
                           is_type_eq(get_type(from_type)->array.contained,
                                      get_type(t)->ptr.ref)) {
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
                for (AstNode *e=n->expr.list.first_child; e != 0; e=e->next_sibling) {
                    bool is_last = e->next_sibling == 0;
                    t = typecheck_expr(prog, scope, e, is_last ? type_hint : TYPE_UNKNOWN);
                }
            }
            break;
        case EXPR_IDENT:
            {
                Variable *var = NULL;
                // could be expression in global scope (ie scope == NULL)
                if (scope) {
                    var = scope_lookup(scope, n->expr.ident);
                }
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
                case LIT_INT_ANY:
                    if (type_hint == U8) {
                        t = U8;
                        n->expr.literal.type = LIT_U8;
                    } else if (type_hint == I8) {
                        t = I8;
                        n->expr.literal.type = LIT_I8;
                    } else if (type_hint == U16) {
                        t = U16;
                        n->expr.literal.type = LIT_U16;
                    } else if (type_hint == I16) {
                        t = I16;
                        n->expr.literal.type = LIT_I16;
                    } else {
                        t = TYPE_UNKNOWN;
                    }
                    check_int_literal_range(n->start_token, t, n->expr.literal.literal_int);
                    break;
                case LIT_U8:
                    t = U8;
                    check_int_literal_range(n->start_token, t, n->expr.literal.literal_int);
                    break;
                case LIT_I8:
                    t = I8;
                    check_int_literal_range(n->start_token, t, n->expr.literal.literal_int);
                    break;
                case LIT_U16:
                    t = U16;
                    check_int_literal_range(n->start_token, t, n->expr.literal.literal_int);
                    break;
                case LIT_I16:
                    t = I16;
                    check_int_literal_range(n->start_token, t, n->expr.literal.literal_int);
                    break;
                case LIT_VOID:
                    t = VOID;
                    break;
                case LIT_BOOL:
                    t = BOOL;
                    break;
                case LIT_STR:
                    // len+1 to accommodate null terminator
                    t = make_array_type(n->expr.literal.literal_str.len+1, U8);
                    break;
                case LIT_ARRAY:
                    {
                        AstNode *item = n->expr.literal.literal_array_first_val;
                        if (item == 0) {
                            fatal_error(n->start_token, "Zero length array literal not permitted");
                        }
                        TypeId item_type_hint = get_type(type_hint)->type == TT_ARRAY ?
                            get_type(type_hint)->array.contained : TT_UNKNOWN;
                        t = typecheck_expr(prog, scope, item, item_type_hint);
                        int len = 1;
                        while (item->next_sibling != 0) {
                            len++;
                            item = item->next_sibling;
                            if (!is_type_eq(t, typecheck_expr(prog, scope, item, item_type_hint))) {
                                fatal_error(item->start_token, "Unmatched array item type");
                            }
                        }
                        t = make_array_type(len, t);
                    }
                    break;
            }
            break;
        case EXPR_CALL:
            t = typecheck_call(prog, scope, n);
            break;
        case EXPR_IF_ELSE:
            {
                TypeId cond = typecheck_expr(prog, scope, n->expr.if_else.condition, U8 /* XXX bool */);

                if (cond != BOOL) {
                    fatal_error(n->start_token, "expected `bool` if condition, but found '%.*s'",
                            (int)get_type(cond)->name.len,
                            get_type(cond)->name.s);
                }

                TypeId on_true = typecheck_expr(prog, scope, n->expr.if_else.on_true, type_hint);

                if (n->expr.if_else.on_false != 0) {
                    TypeId on_false = typecheck_expr(prog, scope, n->expr.if_else.on_false, type_hint);

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
        case EXPR_LOOP:
            {
                if (n->expr.loop.condition != 0) {
                    TypeId cond = typecheck_expr(prog, scope, n->expr.loop.condition, BOOL);

                    if (cond != BOOL) {
                        fatal_error(n->start_token, "expected `bool` while condition, but found '%.*s'",
                                (int)get_type(cond)->name.len,
                                get_type(cond)->name.s);
                    }
                }
                label_push(scope, (JumpLabel) {
                    .label = n->expr.loop.label,
                    .node = n
                });
                if (n->expr.loop.on_next_iter) {
                    typecheck_expr(prog, scope, n->expr.loop.on_next_iter, TYPE_UNKNOWN);
                }
                typecheck_expr(prog, scope, n->expr.loop.body, TYPE_UNKNOWN);
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
                    n->expr.local_scope.var_type = typecheck_expr(prog, scope, n->expr.local_scope.value, TYPE_UNKNOWN);
                }
                check_valid_binding_type(n, n->expr.local_scope.var_type);

                if (scope_lookup(scope, n->expr.local_scope.var_name) != NULL) {
                    fatal_error(n->start_token, "Local variable called '%.*s' already defined",
                            n->expr.local_scope.var_name.len,
                            n->expr.local_scope.var_name.s);
                }
                scope_push(scope, (Variable) {
                    .name = n->expr.local_scope.var_name,
                    .type = n->expr.local_scope.var_type
                });
                t = typecheck_expr(prog, scope, n->expr.local_scope.scoped_expr, type_hint);

                scope_pop(scope);
            }
            break;
        case EXPR_FE_OPERATOR:
            t = typecheck_operator(prog, scope, n, type_hint);
            break;
        case EXPR_RETURN:
            {
                TypeId ret = typecheck_expr(prog, scope, n->expr.return_.val, type_hint);
                validate_return(n->start_token, scope->return_type, ret);
                t = NEVER;
            }
            break;
        case EXPR_BUILTIN:
            // EXPR_OPERATOR nodes should be converted to
            // EXPR_BUILTIN by THIS function, so do not expect
            // to walk over them...
            t = n->expr.eval_type;
            break;
    }
    n->expr.eval_type = t;
    return t;
}

static void typecheck_fn(Program *prog, AstNode *fn) {
    // ignore extern / forward declarations
    if (fn->fn.body == 0) {
        return;
    }

    TypeId expected = get_type(fn->fn.type)->func.ret;
    Scope scope = new_localscope(expected);

    /* Add function arguments to scope */
    for (AstNode *arg=fn->fn.first_arg; arg != 0; arg=arg->next_sibling) {
        assert(arg->type == AST_FN_ARG);
        
        scope_push(&scope, (Variable) {
            .name = arg->fn_arg.name,
            .type = arg->fn_arg.type
        });
    }

    TypeId returned = typecheck_expr(prog, &scope, fn->fn.body, expected);

    validate_return(fn->start_token, scope.return_type, returned);

    free_localscope(&scope);
}

static void check_is_constexpr(Program *prog, AstNode *n) {
    assert(n->type == AST_EXPR);
    if (n->expr.type == EXPR_CAST) {
        // casts to pointer can be compile-time evaluated
        const TypeId cast_to = n->expr.cast.to_type;
        if (get_type(cast_to)->type == TT_PTR) {
            check_is_constexpr(prog, n->expr.cast.arg);
            AstNode *next_sibling = n->next_sibling;
            *n = *n->expr.cast.arg;
            n->next_sibling = next_sibling;
            n->expr.eval_type = cast_to;
        } else {
            fatal_error(n->start_token, "Only casts to pointer can be evaluated in compile-time expressions");
        }
    }
    if (n->expr.type != EXPR_LITERAL) {
        fatal_error(n->start_token, "Expected literal");
    }
    switch (n->expr.literal.type) {
        case LIT_ARRAY:
            check_is_constexpr(prog, n->expr.literal.literal_array_first_val);
            break;
        case LIT_BOOL:
        case LIT_U8:
        case LIT_U16:
        case LIT_I8:
        case LIT_I16:
        case LIT_VOID:
        case LIT_STR:
            break;
        case LIT_INT_ANY:
            fatal_error(n->start_token, "Can not resolve integer type in literal");
    }
}

static void typecheck_global(Program *prog, AstNode *n) {
    assert(n->type == AST_DEF_VAR);

    if (n->var_def.type != TYPE_UNKNOWN) check_valid_binding_type(n, n->var_def.type);

    if (n->var_def.is_const == false && n->var_def.value == 0) return;
    assert(n->var_def.value != 0);

    TypeId v = typecheck_expr(prog, NULL, n->var_def.value, n->var_def.type);
    check_is_constexpr(prog, n->var_def.value);

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

    if (get_type(v)->type == TT_FUNC) {
        fatal_error(n->start_token, "no");
    }

    check_valid_binding_type(n, n->var_def.type);
}

/** deduces the eval_type of AST_EXPR nodes, and checks for type errors. */
void typecheck_program(Program *prog) {
    for (AstNode *node=prog->root->module.first_child; node != 0; node=node->next_sibling) {
        if (node->type == AST_FN) {
            typecheck_fn(prog, node);
        }
        if (node->type == AST_DEF_VAR) {
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
