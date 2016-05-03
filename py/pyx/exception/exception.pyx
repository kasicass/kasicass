cpdef int divide_ints(int i, int j):
	return i / j

cpdef int divide_ints_exception(int i, int j) except? -1:
	return i / j

