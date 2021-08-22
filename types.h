#ifndef TYPES_H
#define TYPES_H

#include "str.h"

typedef int TypeId;

typedef struct Type {
    Str name;
    int size;        /* size of type when packed */
    int stack_size;  /* size of type when pushed on stack */
    int stack_offset; /* when pushed to stack, when does data actually begin? */

    enum TypeType {
        TYPE_PRIM,
        TYPE_ARRAY,
        // TYPE_PTR,
        // TYPE_STRUCT
    } type;

    struct {
        TypeId contained;
    } array;
} Type;

/* TypeId of primitive types (indexes in types vec) */
#define VOID    0
#define U8      1
#define U16     2

void init_types();
TypeId add_type(Type t);
TypeId lookup_type(Str name);
const Type *get_type(TypeId id);
bool is_type_eq(TypeId a, TypeId b);

#endif /* TYPES_H */
