#include <zmq.h>
#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <event.h>

void pair_callback(int _, short __, void *pull)
{
	zmq_msg_t msg;
	uint32_t events;
	size_t len;

	len = sizeof(events);
	zmq_getsockopt(pull, ZMQ_EVENTS, &events, &len);

	if ( events & ZMQ_POLLIN )
	{	
		while (1)
		{
			zmq_msg_init(&msg);
			if ( zmq_recv(pull, &msg, ZMQ_NOBLOCK) == -1 )
			{
				zmq_msg_close(&msg);
				break;
			}
			printf("recv: %s\n", (char *)zmq_msg_data(&msg));
			zmq_msg_close(&msg);
		}
	}
}

void *mythread(void *pull)
{
	struct event_base *evbase;
	struct event *ev;
	int fd;
	size_t len;

	len = sizeof(pull);
	zmq_getsockopt(pull, ZMQ_FD, &fd, &len);
	sleep(1);

	evbase = event_base_new();	
	ev = event_new(evbase, fd, EV_READ|EV_PERSIST, pair_callback, pull);
	event_add(ev, NULL);

	event_base_dispatch(evbase);

	return (void *)0;
}


int main()
{
	void *ctx, *pull, *push;
	pthread_t tid;

	ctx  = zmq_init(0);

	push = zmq_socket(ctx, ZMQ_PUSH);
	zmq_bind(push, "inproc://mygod");

	pull = zmq_socket(ctx, ZMQ_PULL);
	zmq_connect(pull, "inproc://mygod");

	pthread_create(&tid, NULL, mythread, pull);

	char buf[80];
	int i = 0;

	while (1)
	{
		sprintf(buf, "hello %d", i);

		zmq_msg_t msg;
		zmq_msg_init_size(&msg, strlen(buf));
		memcpy(zmq_msg_data(&msg), buf, strlen(buf)+1);
		zmq_send(push, &msg, 0);
		zmq_msg_close(&msg);

		i++;
		sleep(5);
	}


	return 0;
}

