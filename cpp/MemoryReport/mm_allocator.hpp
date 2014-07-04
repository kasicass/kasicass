#pragma once

#include <limits>

namespace mm {

template <typename T, RECORD_TAG tag = TAG_MISC>
class allocator
{
public:
	typedef T value_type;
	typedef value_type* pointer;
	typedef const value_type* const_pointer;
	typedef value_type& reference;
	typedef const value_type& const_reference;
	typedef size_t size_type;
	typedef ptrdiff_t difference_type;

public:
	template <typename U>
	struct rebind { typedef allocator<U> other; };

public:
	inline explicit allocator() {}
	inline ~allocator() {}
	inline explicit allocator(allocator const&) {}
	template <typename U>
	inline explicit allocator(allocator<U> const&) {}

	// address
	inline pointer address(reference r) { return &r; }
	inline const_pointer address(const_reference r) { return &r; }

	// memory allocation
	inline pointer allocate(size_type cnt, const void* hint = 0) {
		if (cnt == 0) return 0;
		size_type sz = cnt * sizeof(T);
		pointer p = reinterpret_cast<pointer>(::operator new(sz));
		if (p) RecordAlloc(tag, sz);
		return p;
	}
	inline void deallocate(pointer p, size_type cnt) {
		if (p) RecordDealloc(tag, cnt * sizeof(T));
		::operator delete(p);
	}

	// size
	inline size_type max_size() const {
		return std::numeric_limits<size_type>::max() / sizeof(T);
	}

	// construction/destruction
	inline void construct(pointer p, const T& t) { new (p) T(t); }
	inline void destroy(pointer p) { p->~T(); }

	inline bool operator==(allocator const&) { return true; }
	inline bool operator!=(allocator const& a) { return !operator==(a); }
};

}

