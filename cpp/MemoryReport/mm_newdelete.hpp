#pragma once

namespace mm {

#if defined(MEMORY_REPORT_ENABLE)

#if 0   // Variadic Templates (vs2013)
template <typename T, typename... Arguments>
T* New(RECORD_TAG tag, Arguments... args)
{
	T* p = (T*)Malloc(tag, sizeof(T));
	::new (p) T(args...);
	return p;
}
#endif

template <typename T>
T* New(RECORD_TAG tag)
{
	T* p = (T*)Malloc(tag, sizeof(T));
	::new (p) T();
	return p;
}

template <typename T, typename X1>
T* New(RECORD_TAG tag, const X1& arg1)
{
	T* p = (T*)Malloc(tag, sizeof(T));
	::new (p) T(arg1);
	return p;
}

template <typename T, typename X1, typename X2>
T* New(RECORD_TAG tag, const X1& arg1, const X2& arg2)
{
	T* p = (T*)Malloc(tag, sizeof(T));
	::new (p) T(arg1, arg2);
	return p;
}

template <typename T, typename X1, typename X2, typename X3>
T* New(RECORD_TAG tag, const X1& arg1, const X2& arg2, const X3& arg3)
{
	T* p = (T*)Malloc(tag, sizeof(T));
	::new (p) T(arg1, arg2, arg3);
	return p;
}

template <typename T, typename X1, typename X2, typename X3, typename X4>
T* New(RECORD_TAG tag, const X1& arg1, const X2& arg2, const X3& arg3, const X4& arg4)
{
	T* p = (T*)Malloc(tag, sizeof(T));
	::new (p) T(arg1, arg2, arg3, arg4);
	return p;
}

template <typename T, typename X1, typename X2, typename X3, typename X4, typename X5>
T* New(RECORD_TAG tag, const X1& arg1, const X2& arg2, const X3& arg3, const X4& arg4, const X5& arg5)
{
	T* p = (T*)Malloc(tag, sizeof(T));
	::new (p) T(arg1, arg2, arg3, arg4, arg5);
	return p;
}

template <typename T, typename X1, typename X2, typename X3, typename X4, typename X5, typename X6>
T* New(RECORD_TAG tag, const X1& arg1, const X2& arg2, const X3& arg3, const X4& arg4, const X5& arg5, const X6& arg6)
{
	T* p = (T*)Malloc(tag, sizeof(T));
	::new (p) T(arg1, arg2, arg3, arg4, arg5, arg6);
	return p;
}

template <typename T, typename X1, typename X2, typename X3, typename X4, typename X5, typename X6, typename X7>
T* New(RECORD_TAG tag, const X1& arg1, const X2& arg2, const X3& arg3, const X4& arg4, const X5& arg5, const X6& arg6, const X7& arg7)
{
	T* p = (T*)Malloc(tag, sizeof(T));
	::new (p) T(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
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

#else

template <typename T>
T* New(RECORD_TAG tag)
{
	return new T();
}

template <typename T, typename X1>
T* New(RECORD_TAG tag, const X1& arg1)
{
	return new T(arg1);
}

template <typename T, typename X1, typename X2>
T* New(RECORD_TAG tag, const X1& arg1, const X2& arg2)
{
	return new T(arg1, arg2);
}

template <typename T, typename X1, typename X2, typename X3>
T* New(RECORD_TAG tag, const X1& arg1, const X2& arg2, const X3& arg3)
{
	return new T(arg1, arg2, arg3);
}

template <typename T, typename X1, typename X2, typename X3, typename X4>
T* New(RECORD_TAG tag, const X1& arg1, const X2& arg2, const X3& arg3, const X4& arg4)
{
	return new T(arg1, arg2, arg3, arg4);
}

template <typename T, typename X1, typename X2, typename X3, typename X4, typename X5>
T* New(RECORD_TAG tag, const X1& arg1, const X2& arg2, const X3& arg3, const X4& arg4, const X5& arg5)
{
	return new T(arg1, arg2, arg3, arg4, arg5);
}

template <typename T, typename X1, typename X2, typename X3, typename X4, typename X5, typename X6>
T* New(RECORD_TAG tag, const X1& arg1, const X2& arg2, const X3& arg3, const X4& arg4, const X5& arg5, const X6& arg6)
{
	return new T(arg1, arg2, arg3, arg4, arg5, arg6);
}

template <typename T, typename X1, typename X2, typename X3, typename X4, typename X5, typename X6, typename X7>
T* New(RECORD_TAG tag, const X1& arg1, const X2& arg2, const X3& arg3, const X4& arg4, const X5& arg5, const X6& arg6, const X7& arg7)
{
	return new T(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}


template <typename T>
void Delete(T* p)
{
	delete p;
}

#endif

}

