all:
	gcc -o fserver -Wall -I/usr/local/include fserver.c -L/usr/local/lib -lzmq -pthread
	gcc -o fclient -Wall -I/usr/local/include fclient.c -L/usr/local/lib -lzmq -pthread

clean:
	rm -rf fserver fclient *.o
