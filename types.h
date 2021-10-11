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
        TT_UNKNOWN,
        TT_NEVER,
        TT_PRIM_VOID,
        TT_PRIM_BOOL,
        TT_PRIM_U8,
        TT_PRIM_I8,
        TT_PRIM_U16,
        TT_PRIM_I16,
        TT_ARRAY,
        TT_FUNC,
        TT_PTR,
        // TT_STRUCT
    } type;

    union {
        struct {
            TypeId contained;
        } array;

        struct {
            Vec /*<TypeId>*/ args;
            TypeId ret;
        } func;

        struct {
            TypeId ref;
        } ptr;
    };
} Type;

/* TypeId of primitive types (indexes in types vec) */
#define TYPE_UNKNOWN 0 // used by AST before expression nodes have a known type
#define NEVER   1
#define VOID    2
#define BOOL    3
#define U8      4
#define I8      5
#define U16     6
#define I16     7

void init_types();
TypeId add_type(Type t);
TypeId lookup_type(Str name);
Type *get_type(TypeId id);
bool is_type_eq(TypeId a, TypeId b);
TypeId make_ptr_type(TypeId ref);
TypeId make_array_type(int num_elems, TypeId contained);

#endif /* TYPES_H */
