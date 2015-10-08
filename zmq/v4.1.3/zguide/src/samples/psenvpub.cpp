#include "zhelper.h"

int main()
{
	void *context = zmq_ctx_new();
	void *publisher = zmq_socket(context, ZMQ_PUB);
	zmq_bind(publisher, "tcp://*:5563");

	while (1) {
		// Write two messages, each with an envelope and content
		s_sendmore(publisher, "A");
		s_send(publisher, "We don't want to see this");
		s_sendmore(publisher, "B");
		s_send(publisher, "We would like to see this");
		s_sleep(1000);
	}

	zmq_close(publisher);
	zmq_ctx_destroy(context);
	return 0;
}