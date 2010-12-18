#include <stdio.h>
#include <zmq.h>

int main()
{
    void *ctx, *pull;
    zmq_msg_t msg;

    ctx = zmq_init(1);
    pull = zmq_socket(ctx, ZMQ_PULL);
    zmq_bind(pull, "tcp://127.0.0.1:8888");

    while (1)
    {
        zmq_msg_init(&msg);
        zmq_recv(pull, &msg, 0);
        printf("recv: %s\n", (char *)zmq_msg_data(&msg));
        zmq_msg_close(&msg);
    }

    return 0;
}

