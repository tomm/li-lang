#include "types.h"
#include "vec.h"
#include "str.h"
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

static Vec/*<Type>*/ types;

const Type *get_type(TypeId id) {
    return vec_get(&types, id);
}

/* -1 = not found */
TypeId lookup_type(Str name) {
    for (TypeId id=0; id<types.len; ++id) {
        if (Str_eq2(get_type(id)->name, name)) return id;
    }
    return -1;
}

bool type_eq(TypeId a, TypeId b) {
    if (get_type(a)->type == TYPE_PRIM && get_type(b)->type == TYPE_PRIM) {
        return a == b;
    } else {
        printf("type_eq not implemented\n");
        abort();
    }
}

TypeId add_type(Type t) {
    int id = types.len;
    vec_push(&types, &t);
    return id;
}

void init_types() {
    types = vec_init(sizeof(Type));

    add_type((Type) {
        /* TYPE_UNKNOWN */
        .name = { .s = "unknown", .len = 7 },
        .size = 0,
        .stack_size = 0,
        .stack_offset = 0,
        .type = TYPE_PRIM
    });

    add_type((Type) {
        /* VOID */
        .name = { .s = "void", .len = 4 },
        .size = 0,
        .stack_size = 0,
        .stack_offset = 0,
        .type = TYPE_PRIM
    });

    add_type((Type) {
        /* U8 */
        .name = { .s = "u8", .len = 2 },
        .size = 1,
        .stack_size = 2,
        .stack_offset = 1, /* because we used `push af` */
        .type = TYPE_PRIM
    });

    add_type((Type) {
        /* U16 */
        .name = { .s = "u16", .len = 3 },
        .size = 2,
        .stack_size = 2,
        .stack_offset = 0,
        .type = TYPE_PRIM
    });
}

bool is_type_eq(TypeId a, TypeId b) {
    const Type *ta = get_type(a);
    const Type *tb = get_type(b);
    switch (ta->type) {
        case TYPE_PRIM:
            return a == b;
        case TYPE_ARRAY:
            return ta->size == tb->size &&
                   ta->type == tb->type &&
                   is_type_eq(ta->array.contained, tb->array.contained);
    }
}
