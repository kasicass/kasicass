#include <stdio.h>
#include <stdlib.h>
#include <uv.h>

#define FIB_UNTIL 25
uv_loop_t *loop;
uv_work_t fib_reqs[FIB_UNTIL];

long fib_(long t)
{
	if (t == 0 || t == 1)
		return 1;
	else
		return fib_(t-1) + fib_(t-2);
}

void fib(uv_work_t *req)
{
	long fib;
	int n = *(int*)req->data;
	if (rand() % 2)
		Sleep(1000);
	else
		Sleep(3*1000);
	fib = fib_(n);
	fprintf(stderr, "%dth fibonacci is %lu\n", n, fib);
}

void after_fib(uv_work_t *req, int status)
{
	if (status == UV_ECANCELED)
		fprintf(stderr, "Calculation of %d cancelled.\n", *(int*)req->data);
}

void signal_handler(uv_signal_t *req, int signum)
{
	int i;

	printf("Signal received!\n");
	for (i = 0; i < FIB_UNTIL; ++i)
	{
		uv_cancel((uv_req_t*)&fib_reqs[i]);
	}
	uv_signal_stop(req);
}

int main()
{
	int data[FIB_UNTIL];
	int i;
	uv_signal_t sig;

	loop = uv_default_loop();
	for (i = 0; i < FIB_UNTIL; ++i)
	{
		data[i] = i;
		fib_reqs[i].data = (void*)&data[i];
		uv_queue_work(loop, &fib_reqs[i], fib, after_fib);
	}

	uv_signal_init(loop, &sig);
	uv_signal_start(&sig, signal_handler, SIGINT);

	return uv_run(loop, UV_RUN_DEFAULT);
}