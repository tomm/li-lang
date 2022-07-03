#ifndef SLABALLOC_H
#define SLABALLOC_H

#include <stdlib.h>

struct SlabAlloc;

struct SlabAlloc *slab_allocator_new(size_t item_size, int32_t nitems);
void slab_allocator_free(struct SlabAlloc *);
void *slab_alloc_item(struct SlabAlloc *);

#endif /* SLABALLOC_H */
