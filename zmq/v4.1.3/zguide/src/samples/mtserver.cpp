// Multithreaded Hello World server

#include "zhelper.h"
#include "zhelper_win32.h"

class WorkerThread : public OSThread
{
public:
	WorkerThread(int no, void *context) : no_(no), context_(context) {}

private:
	virtual void workFunc()
	{
		// Socket to talk to dispatcher
		void *receiver = zmq_socket(context_, ZMQ_REP);
		zmq_connect(receiver, "inproc://workers");

		while (1)
		{
			char *string = s_recv(receiver);
			printf("#%d Received request: [%s]\n", no_, string);
			free(string);

			s_sleep(1);

			s_send(receiver, "World");
		}

		zmq_close(receiver);
	}

private:
	int no_;
	void *context_;
};

int main()
{
	void *context = zmq_ctx_new();

	// Socket to talk to clients
	void *clients = zmq_socket(context, ZMQ_ROUTER);
	zmq_bind(clients, "tcp://*:5555");

	// Socket to talk to workers
	void *workers = zmq_socket(context, ZMQ_DEALER);
	zmq_bind(workers, "inproc://workers");

	// Launch pool of worker threads
	const int THREAD_NUM = 5;
	int thread_nbr;
	WorkerThread *threads[THREAD_NUM];
	for (thread_nbr = 0; thread_nbr < THREAD_NUM; ++thread_nbr)
	{
		threads[thread_nbr] = new WorkerThread(thread_nbr, context);
		threads[thread_nbr]->run();
	}

	zmq_proxy(clients, workers, NULL);

	for (thread_nbr = 0; thread_nbr < THREAD_NUM; ++thread_nbr)
	{
		delete threads[thread_nbr];
	}

	zmq_close(clients);
	zmq_close(workers);
	zmq_ctx_destroy(context);
	return 0;
}