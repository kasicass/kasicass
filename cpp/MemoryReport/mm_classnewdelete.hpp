#pragma once

namespace mm {

#if defined(MEMORY_REPORT_ENABLE)

template <RECORD_TAG tag>
class ClassNewDelete
{
public:
	inline void* operator new(size_t sz)
	{
		return Malloc(tag, sz);
	}

	inline void operator delete(void* p)
	{
		Free(p);
	}

	inline void * operator new[](size_t sz) 
	{
		return Malloc(sz);
	}

	void operator delete[](void *p)
	{
		Free(p);
	}
};

#else

template <RECORD_TAG tag>
class ClassNewDelete
{
};

#endif

};

