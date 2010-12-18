all:
	gcc -g -o svr -Wall -I/usr/local/include server.c -L/usr/local/lib -lzmq -pthread
	gcc -g -o cli -Wall -I/usr/local/include client.c -L/usr/local/lib -lzmq -pthread

clean:
	rm -rf svr cli *.o
