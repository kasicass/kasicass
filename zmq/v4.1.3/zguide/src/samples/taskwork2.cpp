// Task worker - design 2
// Adds pub-sub flow to receive and respond to kill signal

#include "zhelper.h"

int main()
{
	// Socket to receive messages on
	void *context = zmq_ctx_new();
	void *receiver = zmq_socket(context, ZMQ_PULL);
	zmq_connect(receiver, "tcp://localhost:5557");

	// Socket to send messages to
	void *sender = zmq_socket(context, ZMQ_PUSH);
	zmq_connect(sender, "tcp://localhost:5558");

	// Socket for control input
	void *controller = zmq_socket(context, ZMQ_SUB);
	zmq_connect(controller, "tcp://localhost:5559");
	zmq_setsockopt(controller, ZMQ_SUBSCRIBE, "", 0);

	while (1)
	{
		zmq_pollitem_t items[] = {
			{ receiver, 0, ZMQ_POLLIN, 0 },
			{ controller, 0, ZMQ_POLLIN, 0 },
		};
		zmq_poll(items, 2, -1);
		if (items[0].revents & ZMQ_POLLIN)
		{
			char *string = s_recv(receiver);
			printf("%s.", string);
			fflush(stdout);
			s_sleep(atoi(string));
			free(string);
			s_send(sender, "");
		}
		if (items[1].revents & ZMQ_POLLIN)
			break;
	}

	zmq_close(receiver);
	zmq_close(sender);
	zmq_close(controller);
	zmq_ctx_destroy(context);
	return 0;
}
