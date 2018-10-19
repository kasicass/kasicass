# make
# python mytest.py
# pypy mytest.py

import cffi
from os import path

dllname = path.join(path.abspath('.'), 'libhello.so')

ffi = cffi.FFI()
ffi.cdef('''
	void myhello(const char* s);
''')

DLL = ffi.dlopen(dllname)
myhello = DLL.myhello

myhello('Woo, cffi!')

