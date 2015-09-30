// Hello World worker
// Connects REP socket to tcp://localhost:5560
// Expects "Hello" from client, replies with "World"

#include "zhelper.h"

int main()
{
	void *context = zmq_ctx_new();

	// Socket to talk to clients
	void *responder = zmq_socket(context, ZMQ_REP);
	zmq_connect(responder, "tcp://localhost:5560");

	while (1)
	{
		// Wait for next request from client
		char *string = s_recv(responder);
		printf("Received request: [%s]\n", string);
		free(string);

		s_sleep(1);

		s_send(responder, "World");
	}

	zmq_close(responder);
	zmq_ctx_destroy(context);
	return 0;
}