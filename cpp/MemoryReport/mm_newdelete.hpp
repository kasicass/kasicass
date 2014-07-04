#pragma once

namespace mm {

template <typename T>
T* New(RECORD_TAG tag)
{
	T* p = (T*)Malloc(tag, sizeof(T));
	::new (p) T();
	return p;
}

template <typename T>
void Delete(T* p)
{
	if (p != 0)
	{
		p->~T();
		Free(p);
	}
}

}

