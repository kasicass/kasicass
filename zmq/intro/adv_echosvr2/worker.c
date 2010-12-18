#include <stdio.h>
#include <zmq.h>

int main()
{
    void *ctx, *reply;
    zmq_msg_t msg;

    ctx = zmq_init(1);
    reply = zmq_socket(ctx, ZMQ_REP);
    zmq_connect(reply, "ipc://workers");

    printf("worker %p\n", reply);
    while (1)
    {
        zmq_msg_init(&msg);
        zmq_recv(reply, &msg, 0);
        printf("%p recv: %s\n", reply, (char *)zmq_msg_data(&msg));

        zmq_send(reply, &msg, 0);
        zmq_msg_close(&msg);
    }

    zmq_term(ctx);
    return 0;
}

