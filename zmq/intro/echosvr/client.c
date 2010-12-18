// ./cli 127.0.0.1:8888 text
#include <zmq.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[])
{
    void *ctx, *request;
    zmq_msg_t msg;
    size_t mlen;
    char addr[80];

    // init
    ctx = zmq_init(1);
    request = zmq_socket(ctx, ZMQ_REQ);
    sprintf(addr, "tcp://%s", argv[1]);
    zmq_connect(request, addr);

    // send
    mlen = strlen(argv[2])+1;
    zmq_msg_init_size(&msg, mlen);
    memcpy(zmq_msg_data(&msg), argv[2], mlen);

    zmq_send(request, &msg, 0);
    zmq_msg_close(&msg);

    // recv
    zmq_msg_init(&msg);
    zmq_recv(request, &msg, 0);
    printf("echo: %s\n", (char *)zmq_msg_data(&msg));
    zmq_msg_close(&msg);

    // close socket
    zmq_close(request);
    zmq_term(ctx);

    return 0;
}

