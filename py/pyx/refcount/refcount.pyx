import gc

def test():
	b1 = b"hello1"
	b2 = b" bytes"
	cdef bytes cb = b1 + b2
	cdef char *p = cb
	print p
