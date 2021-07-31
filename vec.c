#include "vec.h"
#include <assert.h>
#include <string.h>

#ifndef MAX
# define MAX(a,b)		((a)>(b) ? (a) : (b))
# define MIN(a,b)		((a)<(b) ? (a) : (b))
#endif

Vec vec_init(size_t sizeof_elem) {
    return (Vec) { NULL, sizeof_elem, 0, 0 };
}

void vec_free(Vec *v) {
    free(v->data);
    v->data = NULL;
}

void *vec_get(Vec *v, size_t index) {
    return (unsigned char*)v->data + (index * v->elem_size);
}

void vec_set(Vec *v, size_t index, const void *elem) {
    memcpy(vec_get(v, index), elem, v->elem_size);
}

void vec_resize(Vec *v, size_t num_elems)
{
    v->data = realloc(v->data, num_elems * v->elem_size);
    v->elems_allocd = num_elems;
    v->len = MIN(v->len, num_elems);
}

static void _grow(Vec *v, int num_new_elems) {
    int elems_free = v->elems_allocd - v->len;

    if (v->data == NULL) {
        //printf("Growing from %ld to %d\n", v->elems_allocd, num_new_elems);
        v->data = malloc(num_new_elems * v->elem_size);
        v->elems_allocd = num_new_elems;
    } else if (elems_free < num_new_elems) {
        int new_size = MAX(v->elems_allocd + num_new_elems, v->elems_allocd * 2);
        //printf("Growing from %ld to %d\n", v->elems_allocd, new_size);
        v->data = realloc(v->data, new_size * v->elem_size);
        v->elems_allocd = new_size;
    }
}

void vec_concat(Vec *v, const void *elems, size_t num_elems) {
    _grow(v, num_elems);
    memcpy(vec_get(v, v->len), elems, v->elem_size * num_elems);
    v->len += num_elems;
}

void vec_push(Vec *v, const void *elem) {
    _grow(v, 1);
    vec_set(v, v->len, elem);
    v->len++;
}

void vec_pop(Vec *v, void *popped) {
    assert(v->len > 0);
    v->len--;
    memcpy(popped, vec_get(v, v->len), v->elem_size);
}

#ifdef TEST
#include <stdio.h>

int main() {
    fprintf(stderr, "Testing Vec... ");

    {
        // vector of 4 byte items
        Vec v = vec_init(4);
        vec_push(&v, "Tom");
        assert(memcmp(vec_get(&v, 0), "Tom", 4) == 0);

        char buf[4];
        vec_pop(&v, buf);
        assert(memcmp(buf, "Tom", 4) == 0);

        vec_push(&v, "Bob1");
        vec_push(&v, "Bob2");
        vec_push(&v, "Bob3");
        assert(memcmp(vec_get(&v, 0), "Bob1", 4) == 0);
        assert(memcmp(vec_get(&v, 1), "Bob2", 4) == 0);
        assert(memcmp(vec_get(&v, 2), "Bob3", 4) == 0);

        vec_free(&v);
    }

    {
        // vector of chars
        Vec v = vec_init(sizeof(char));
        vec_concat(&v, "Hello ", 6);
        vec_concat(&v, "World", 5);
        vec_concat(&v, "!", 1);
        assert(v.len == 12);
        assert(memcmp(v.data, "Hello World!", 12) == 0);
        vec_resize(&v, 5);
        vec_concat(&v, ", friend", 8);
        assert(v.len == 13);
        assert(memcmp(v.data, "Hello, friend", 13) == 0);
        vec_free(&v);
    }

    fprintf(stderr, "Done.\n");

    return 0;
}

#endif /* TEST */
