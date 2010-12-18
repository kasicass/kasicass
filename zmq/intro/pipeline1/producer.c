#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <zmq.h>

int main()
{
    int i;
    char text[80];
    void *ctx, *push;
    zmq_msg_t msg;

    ctx = zmq_init(1);
    push = zmq_socket(ctx, ZMQ_PUSH);
    zmq_bind(push, "tcp://127.0.0.1:8888");

    i = 0;
    while (1)
    {
        sprintf(text, "msg #%d", i);

        zmq_msg_init_size(&msg, strlen(text)+1);
        memcpy(zmq_msg_data(&msg), text, strlen(text)+1);
        zmq_send(push, &msg, 0);
        zmq_msg_close(&msg);

        i++;
        sleep(1);
    }

    return 0;
}

