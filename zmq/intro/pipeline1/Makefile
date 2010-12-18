all:
	gcc -o producer -Wall -I/usr/local/include producer.c -L/usr/local/lib -lzmq -pthread
	gcc -o consumer -Wall -I/usr/local/include consumer.c -L/usr/local/lib -lzmq -pthread

clean:
	rm -rf producer consumer *.o
