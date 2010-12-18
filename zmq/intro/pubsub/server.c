#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <zmq.h>

#define CHANNEL_NUM  3

int main()
{
    void *ctx, *pub;
    zmq_msg_t msg;
    char *channels[CHANNEL_NUM] = {"bbc", "voa", "cctv"};
    char text[80];
    int i;

    ctx = zmq_init(1);
    pub = zmq_socket(ctx, ZMQ_PUB);
    zmq_bind(pub, "tcp://*:8888");

    printf("listen on *:8888 ...\n");

    i = 0;
    while (1)
    {
        sprintf(text, "%s: sth happen!", channels[i]);
        if (++i == CHANNEL_NUM) i = 0;

        zmq_msg_init_size(&msg, strlen(text)+1);
        memcpy(zmq_msg_data(&msg), text, strlen(text)+1);
        zmq_send(pub, &msg, 0);
        zmq_msg_close(&msg);

        sleep(1);
    }

    return 0;
}

