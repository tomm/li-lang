#ifndef TYPES_H
#define TYPES_H

#include "str.h"
#include "vec.h"

typedef int TypeId;

typedef struct Type {
    Str name;
    int size;        /* size of type when packed */
    int stack_size;  /* size of type when pushed on stack */
    int stack_offset; /* when pushed to stack, when does data actually begin? */

    enum TypeType {
        TYPE_PRIM,
        TYPE_ARRAY,
        TYPE_FUNC,
        // TYPE_PTR,
        // TYPE_STRUCT
    } type;

    union {
        struct {
            TypeId contained;
        } array;

        struct {
            Vec /*<TypeId>*/ args;
        } func;
    };
} Type;

/* TypeId of primitive types (indexes in types vec) */
#define TYPE_UNKNOWN 0 // used by AST before expression nodes have a known type
#define VOID    1
#define U8      2
#define U16     3

void init_types();
TypeId add_type(Type t);
TypeId lookup_type(Str name);
Type *get_type(TypeId id);
bool is_type_eq(TypeId a, TypeId b);

#endif /* TYPES_H */
