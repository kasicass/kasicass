#include <stdio.h>
#include <zmq.h>

int main()
{
    void *ctx, *xreply, *xrequest;

    // init
    ctx = zmq_init(1);

    xreply = zmq_socket(ctx, ZMQ_XREP);
    zmq_bind(xreply, "tcp://127.0.0.1:8888");

    xrequest = zmq_socket(ctx, ZMQ_XREQ);
    zmq_bind(xrequest, "ipc://workers");

    printf("listen on 127.0.0.1:8888 ...\n");
    zmq_device (ZMQ_QUEUE, xreply, xrequest);

    zmq_term(ctx);
    return 0;
}

