#include "slaballoc.h"

#include <stdio.h>
#include <stdint.h>

//#define TEST

struct SlabAlloc {
	uint32_t item_size;
	uint32_t num_items;
	uint32_t lowest_free;
	uint32_t _pad;
	struct SlabAlloc *next;
};


void slab_allocator_free(struct SlabAlloc *slab)
{
	struct SlabAlloc *next = slab->next;

#ifdef TEST
	printf("Freeing slab %p (size %d)\n", slab, slab->num_items);
#endif /* TEST */
	free(slab);
	if (next) slab_allocator_free(next);
}

struct SlabAlloc *slab_allocator_new(size_t item_size, int32_t num_items)
{
	num_items = num_items > 0 ? num_items : 1;
	struct SlabAlloc *slab = (struct SlabAlloc*)malloc(sizeof(struct SlabAlloc) + num_items*item_size);
#ifdef TEST
	printf("Creating new slab %p of num_items %d\n", slab, num_items);
#endif /* TEST */

	slab->item_size = item_size;
	slab->num_items = num_items;
	slab->lowest_free = 0;
	slab->next = 0;
	return slab;
}

void *slab_alloc_item(struct SlabAlloc *slab)
{
	if (slab->lowest_free < slab->num_items) {
#ifdef TEST
		printf("Allocating item in slab %p, index %d\n", slab, slab->lowest_free);
#endif /* TEST */
		// &slab[1] skips slab header (struct SlabAlloc) to get to data
		void *item = ((int8_t*)&slab[1]) + slab->item_size*slab->lowest_free;
		slab->lowest_free++;
		return item;
	} else if (slab->next) {
		return slab_alloc_item(slab->next);
	} else {
		slab->next = slab_allocator_new(slab->item_size, slab->num_items*2);
		return slab_alloc_item(slab->next);
	}
}

#ifdef TEST
#define TEST_LEN 10
int main()
{
	int *items[TEST_LEN];
	struct SlabAlloc *a = slab_allocator_new(sizeof(int), 0);
	for (int i=0; i<TEST_LEN; ++i) {
		items[i] = slab_alloc_item(a);
		*items[i] = i;
	}
	for (int i=0; i<TEST_LEN; ++i) {
		printf("item[%d] (%p) = %d\n", i, items[i], *items[i]);
	}
	slab_allocator_free(a);
}
#endif /* TEST */
