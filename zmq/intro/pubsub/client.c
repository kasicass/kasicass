#include <stdio.h>
#include <string.h>
#include <zmq.h>

int main(int argc, char *argv[])
{
    void *ctx, *sub;
    zmq_msg_t msg;

    ctx = zmq_init(1);
    sub = zmq_socket(ctx, ZMQ_SUB);
    zmq_connect(sub, "tcp://127.0.0.1:8888");
    if ( argc > 1 )
    {
        int i;
        for ( i = 1; i < argc; i++ )
            zmq_setsockopt(sub, ZMQ_SUBSCRIBE, argv[i], strlen(argv[i]));
    }
    else
    {
        zmq_setsockopt(sub, ZMQ_SUBSCRIBE, "", 0);
    }

    while (1)
    {
        zmq_msg_init(&msg);
        zmq_recv(sub, &msg, 0);
        printf("%s\n", (char *)zmq_msg_data(&msg));
        zmq_msg_close(&msg);
    }

    return 0;
}

