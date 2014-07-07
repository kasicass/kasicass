#include "mm_report.hpp"
#include <assert.h>

namespace mm {

struct MallocOverhead {
	size_t sz;
	RECORD_TAG tag;
};

#define MM_MALLOC_MASK 0x80000000
void* Malloc(RECORD_TAG tag, size_t sz)
{
	assert((sz & MM_MALLOC_MASK) == 0);

	void *p = malloc(sz + sizeof(MallocOverhead));
	if (p)
	{
		MallocOverhead *mo = (MallocOverhead*)p;
		mo->sz  = (sz | MM_MALLOC_MASK);
		mo->tag = tag;

		RecordAlloc(tag, sz);
		RecordAlloc(TAG_MM_SELF, sizeof(MallocOverhead));

		return ((MallocOverhead *)p + 1);
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
		MallocOverhead *mo = ((MallocOverhead*)p - 1);
		assert((mo->sz & MM_MALLOC_MASK) == MM_MALLOC_MASK);
		mo->sz = (mo->sz & ~MM_MALLOC_MASK);

		RecordDealloc(TAG_MM_SELF, sizeof(MallocOverhead));
		RecordDealloc(mo->tag, mo->sz);

		free(mo);
	}
}

}

