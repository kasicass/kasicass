#pragma once

namespace mm {

template <typename T>
T* New()
{
	T* p = new T();
	if (p) RecordAlloc(sizeof(T));
	return p;
}

template <typename T>
void Delete(T* p)
{
	if (p) RecordDealloc(sizeof(T));
	delete p;
}

}

