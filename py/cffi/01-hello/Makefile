all:
	clang -o hello.o -Wall -fPIC -c hello.c
	clang -o libhello.so -shared hello.o

clean:
	rm -rf *.o *.so
