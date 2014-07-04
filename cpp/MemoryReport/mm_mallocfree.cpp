#include "mm_report.hpp"
#include <assert.h>

namespace mm {

#define MALLOC_TAG 0x80000000
void* Malloc(size_t sz)
{
	assert((sz & MALLOC_TAG) == 0);

	void *p = malloc(sz + sizeof(size_t));
	if (p)
	{
		*((size_t*)p) = (sz | MALLOC_TAG);
		RecordAlloc(sz);
		return (size_t*)p + 1;
	}
	else
	{
		return 0;
	}
}

void Free(void* p)
{
	if (p)
	{
		size_t sz = *((size_t*)p - 1);
		assert((sz & MALLOC_TAG) == MALLOC_TAG);
		sz = (sz & ~MALLOC_TAG);
		RecordDealloc(sz);
		free(((size_t*)p - 1));
	}
}

}

