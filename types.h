#ifndef TYPES_H
#define TYPES_H

#include "str.h"

typedef struct Type {
    Str name;
    int size;        /* size of type when packed */
    int stack_size;  /* size of type when pushed on stack */

    enum TypeType {
        TYPE_PRIM,
        // TYPE_PTR,
        // TYPE_ARRAY,
        // TYPE_STRUCT
    } type;
} Type;

typedef int TypeId;

/* TypeId of primitive types (indexes in types vec) */
#define VOID    0
#define U8  1

void init_types();
TypeId add_type(Type t);
TypeId lookup_type(Str name);
const Type *get_type(TypeId id);

#endif /* TYPES_H */