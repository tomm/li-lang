#ifndef __VEC_H
#define __VEC_H

#include <stdlib.h>

typedef struct Vec Vec;

struct Vec {
    void *data;
    size_t elem_size;
    size_t elems_allocd;
    size_t len;
};

extern Vec vec_init(size_t sizeof_elem);
extern void vec_free(Vec *v);
extern void *vec_get(Vec *v, size_t index);
extern void vec_set(Vec *v, size_t index, const void *elem);
extern void vec_push(Vec *v, const void *elem);
extern void vec_pop(Vec *v, void *popped);
extern void vec_concat(Vec *v, const void *elems, size_t num_elems);
extern void vec_resize(Vec *v, size_t num_elems);

#endif /* __VEC_H */
