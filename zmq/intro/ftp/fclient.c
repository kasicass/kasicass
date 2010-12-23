#include <zmq.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>

void myfree(void *data, void *hint)
{
        free(data);
}

#define MY_BUF_SIZE     1024

int main(int argc, char *argv[])
{
	FILE *fp;
	char *buf;
	size_t n;
	zmq_msg_t msg;
	void *ctx, *s;
	int more;

	if ( argc != 2 )
	{
		fprintf(stderr, "./fclient <file>\n");
		return 1;
	}

	fp = fopen(argv[1], "rb");
	if ( fp == NULL )
	{
		fprintf(stderr, "open file fail: %d\n", errno);
		return 2;
	}

	ctx = zmq_init(1);
	s   = zmq_socket(ctx, ZMQ_REQ);
	zmq_connect(s, "tcp://127.0.0.1:5555");

	// send filename
	zmq_msg_init_size(&msg, strlen(argv[1])+1);
	memcpy(zmq_msg_data(&msg), argv[1], strlen(argv[1])+1);
	zmq_send(s, &msg, 0);
	zmq_msg_close(&msg);

	zmq_msg_init(&msg);
	zmq_recv(s, &msg, 0);
	zmq_msg_close(&msg);

	// content
	while (1)
	{
		buf = (char *)malloc(MY_BUF_SIZE);
		n = fread(buf, 1, MY_BUF_SIZE, fp);
		if ( n == 0 )
			break;

		more = (n == MY_BUF_SIZE);

		zmq_msg_init_data(&msg, buf, n, myfree, NULL);
		zmq_send(s, &msg, more ? ZMQ_SNDMORE : 0);
		zmq_msg_close(&msg);

		if ( !more )
			break;
	}

	zmq_msg_init(&msg);
	zmq_recv(s, &msg, 0);
	zmq_msg_close(&msg);

	zmq_close(s);
	zmq_term(ctx);

	fclose(fp);
	puts("done.");
	return 0;
}

