#pragma once

namespace mm {

#if defined(MEMORY_REPORT_ENABLE)

template <RECORD_TAG tag>
class ClassNewDelete
{
public:

	inline void* operator new(size_t sz)
	{
		void *p = Malloc(tag, sz);
		if (p) RecordIncCount(tag, 1);
		return p;
	}

	inline void operator delete(void* p)
	{
		if (p) RecordDecCount(tag, 1);
		Free(p);
	}

	inline void * operator new[](size_t sz) 
	{
		void *p = Malloc(tag, sz);
		if (p) RecordIncCount(tag, sz / sizeof(T));
		return p;
	}

	void operator delete[](void *p)
	{
		if (p) RecordDecCount(tag, sz / sizeof(T));
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

