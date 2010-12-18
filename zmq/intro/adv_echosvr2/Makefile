all:
	gcc -g -o svr -Wall -I/usr/local/include server.c -L/usr/local/lib -lzmq -pthread
	gcc -g -o worker -Wall -I/usr/local/include worker.c -L/usr/local/lib -lzmq -pthread

clean:
	rm -rf svr worker workers *.o
