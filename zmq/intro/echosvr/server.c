#include <stdio.h>
#include <zmq.h>

int main()
{
    void *ctx, *reply;
    zmq_msg_t msg;

    ctx = zmq_init(1);
    reply = zmq_socket(ctx, ZMQ_REP);
    zmq_bind(reply, "tcp://127.0.0.1:8888");
    zmq_bind(reply, "tcp://127.0.0.1:9999");

    printf("listen on 127.0.0.1:8888/9999 ...\n");
    while (1)
    {
        zmq_msg_init(&msg);
        zmq_recv(reply, &msg, 0);  // 0 means no flags, block recv

        printf("recv = %s\n", (char *)zmq_msg_data(&msg));
        zmq_send(reply, &msg, 0);  // echo it

        zmq_msg_close(&msg);
    }

    zmq_close(reply);
    zmq_term(ctx);
    return 0;
}

