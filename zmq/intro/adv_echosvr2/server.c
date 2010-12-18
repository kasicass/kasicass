#include <sys/types.h>
#include <stdio.h>
#include <assert.h>
#include <zmq.h>

void forwarder_run(void *xreply, void *xrequest)
{
    int64_t more;
    size_t more_size;
    zmq_msg_t msg;

    zmq_pollitem_t items[] = {
        { xreply,   0, ZMQ_POLLIN, 0 },
        { xrequest, 0, ZMQ_POLLIN, 0 },
    };

    while (1)
    {
        zmq_poll(items, 2, 0);

        // data in
        if (items[0].revents & ZMQ_POLLIN)
        {
            while (1)
            {
                zmq_msg_init(&msg);
                zmq_recv(xreply, &msg, 0);

                more_size = sizeof(more);
                zmq_getsockopt(xreply, ZMQ_RCVMORE, &more, &more_size);

                zmq_send(xrequest, &msg, more ? ZMQ_SNDMORE : 0);
                zmq_msg_close(&msg);

                if (!more)
                    break;
            }
        }

        // data out
        if (items[1].revents & ZMQ_POLLIN)
        {
            while (1)
            {
                zmq_msg_init(&msg);
                zmq_recv(xrequest, &msg, 0);

                more_size = sizeof(more);
                zmq_getsockopt(xrequest, ZMQ_RCVMORE, &more, &more_size);

                zmq_send(xreply, &msg, more ? ZMQ_SNDMORE : 0);
                zmq_msg_close(&msg);

                if (!more)
                    break;
            }
        }
    }
}

int main()
{
    int rc;
    void *ctx, *xreply, *xrequest;

    // init
    ctx = zmq_init(1);

    xreply = zmq_socket(ctx, ZMQ_XREP);
    rc = zmq_bind(xreply, "tcp://127.0.0.1:8888");
    assert(rc == 0);

    xrequest = zmq_socket(ctx, ZMQ_XREQ);
    rc = zmq_bind(xrequest, "ipc://workers");
    assert(rc == 0);

    printf("listen on 127.0.0.1:8888 ...\n");
    forwarder_run(xreply, xrequest);

    zmq_term(ctx);
    return 0;
}

