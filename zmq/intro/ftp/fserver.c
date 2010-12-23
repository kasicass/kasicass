#include <zmq.h>
#include <sys/types.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <libgen.h>

void recv_file(void *s)
{
        const char *filename;
        FILE *fp;
        zmq_msg_t msg;
        int64_t more;
        size_t more_size;

        /* filename */
        zmq_msg_init(&msg);
        zmq_recv(s, &msg, 0);

        filename = basename((const char *)zmq_msg_data(&msg));
        printf("begin: %s ...", filename);
        fp = fopen(filename, "wb+");
        zmq_msg_close(&msg);

        zmq_msg_init_size(&msg, 3);
        memcpy(zmq_msg_data(&msg), "ok", 3);
        zmq_send(s, &msg, 0);
        zmq_msg_close(&msg);

        /* file content */
        while (1)
        {
                zmq_msg_init(&msg);
                zmq_recv(s, &msg, 0);

                fwrite((const char *)zmq_msg_data(&msg), zmq_msg_size(&msg), 1, fp);
                zmq_msg_close(&msg);

                zmq_getsockopt(s, ZMQ_RCVMORE, &more, &more_size);
                if (!more)
                        break;
        }
        fclose(fp);

        zmq_msg_init_size(&msg, 3);
        memcpy(zmq_msg_data(&msg), "ok", 3);
        zmq_send(s, &msg, 0);
        zmq_msg_close(&msg);

        puts("done.");
}

int main()
{
        int rc;
        void *ctx, *s;

        ctx = zmq_init(1);
        assert(ctx);

        s = zmq_socket(ctx, ZMQ_REP);
        assert(s);

        rc = zmq_bind(s, "tcp://*:5555");
        assert(rc == 0);

        while (1)
        {
                recv_file(s);
        }

        return 0;
}
