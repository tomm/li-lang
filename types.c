#include "types.h"
#include "vec.h"
#include "str.h"
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

static Vec/*<Type>*/ types;

Type *get_type(TypeId id) {
    return vec_get(&types, id);
}

/* -1 = not found */
TypeId lookup_type(Str name) {
    for (TypeId id=0; id<types.len; ++id) {
        if (Str_eq2(get_type(id)->name, name)) return id;
    }
    return -1;
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
        .type = TT_UNKNOWN
    });

    add_type((Type) {
        /* NEVER */
        .name = { .s = "never", .len = 5 },
        .size = 0,
        .stack_size = 0,
        .stack_offset = 0,
        .type = TT_NEVER
    });

    add_type((Type) {
        /* VOID */
        .name = { .s = "void", .len = 4 },
        .size = 0,
        .stack_size = 0,
        .stack_offset = 0,
        .type = TT_PRIM_VOID
    });

    add_type((Type) {
        /* U8 */
        .name = { .s = "u8", .len = 2 },
        .size = 1,
        .stack_size = 2,
        .stack_offset = 1, /* because we used `push af` */
        .type = TT_PRIM_U8
    });

    add_type((Type) {
        /* U16 */
        .name = { .s = "u16", .len = 3 },
        .size = 2,
        .stack_size = 2,
        .stack_offset = 0,
        .type = TT_PRIM_U16
    });
}

TypeId make_ptr_type(TypeId ref) {
    // XXX could reuse types. instead we always make a new one
    char buf[256];
    snprintf(buf, sizeof(buf), "&%.*s", (int)get_type(ref)->name.len, get_type(ref)->name.s);
    return add_type((Type) {
        .type = TT_PTR,
        .name = { .s = strdup(buf), .len = strlen(buf) },
        .size = 2,
        .stack_size = 2,
        .stack_offset = 0,
        .ptr = {
            .ref = ref
        }
    });
}

TypeId make_array_type(int num_elems, TypeId contained) {
    char buf[256];
    snprintf(buf, sizeof(buf), "[%.*s; %d]",
            (int)get_type(contained)->name.len,
            get_type(contained)->name.s,
            num_elems);
    // XXX this is never deallocated (but the compiler has no teardown anyhow...)
    char *type_name = strdup(buf);
    const int byte_size = get_type(contained)->size * num_elems;
    return add_type((Type) {
        .type = TT_ARRAY,
        .name = { .s = type_name, .len = strlen(type_name) },
        .size = byte_size,
        .stack_size = byte_size,
        .stack_offset = 0,
        .array = {
            .contained = contained
        }
    });
}

/* XXX bad name. is really "does b fulfill requirements of a"
 */
bool is_type_eq(TypeId a, TypeId b) {
    Type *ta = get_type(a);
    Type *tb = get_type(b);
    // never type is always ok
    if ((ta->type != TT_UNKNOWN) && (tb->type == TT_NEVER)) return true;

    switch (ta->type) {
        case TT_UNKNOWN:
            return false;
        case TT_NEVER:
        case TT_PRIM_VOID:
        case TT_PRIM_U8:
        case TT_PRIM_U16:
            return a == b;
        case TT_PTR:
            return tb->type == TT_PTR &&
                   is_type_eq(ta->ptr.ref, tb->ptr.ref);
        case TT_ARRAY:
            return ta->size == tb->size &&
                   ta->type == tb->type &&
                   is_type_eq(ta->array.contained, tb->array.contained);
        case TT_FUNC:
            if (ta->func.args.len != tb->func.args.len) return false;
            for (int i=0; i<ta->func.args.len; ++i) {
                if (!is_type_eq(
                            *(TypeId*)vec_get(&ta->func.args, i),
                            *(TypeId*)vec_get(&tb->func.args, i))) {
                    return false;
                }
            }
            return true;
    }
    assert(false);
}
