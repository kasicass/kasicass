// Simple request-reply broker

#include "zhelper.h"

int main()
{
	// Prepare our context and sockets
	void *context = zmq_ctx_new();
	void *frontend = zmq_socket(context, ZMQ_ROUTER);
	void *backend  = zmq_socket(context, ZMQ_DEALER);
	zmq_bind(frontend, "tcp://*:5559");
	zmq_bind(backend, "tcp://*:5560");

	zmq_proxy(frontend, backend, NULL);

	// We never get here, but cleanup anyhow
	zmq_close(frontend);
	zmq_close(backend);
	zmq_ctx_destroy(context);
	return 0;
}