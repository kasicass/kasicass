def hello_pytype():
	cdef list mylist = [1,2,3]
	print mylist
	del mylist[0]
	print mylist

	cdef dict mydict = {1:"hello", 2:"baby"}
	print mydict.values()

