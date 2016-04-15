def hello_pointer():
	cdef double v;
	cdef double *p = &v;
	p[0] = 1.2   # *p = 1.2
	print p[0]
