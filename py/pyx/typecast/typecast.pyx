
def print_address(a):
	cdef void *v = <void*>a
	cdef long addr = <long>v
	print "Cython address:", addr
	print "Python id     :", id(a)

