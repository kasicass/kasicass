#include <stdio.h>
#include <stdlib.h>
#include <uv.h>
#include <Windows.h>

#define FIB_UNTIL 25
uv_loop_t *loop;

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
	fprintf(stderr, "%dth fibonacci is %lu (%lu)\n", n, fib, GetCurrentThreadId());
}

void after_fib(uv_work_t *req, int status)
{
	fprintf(stderr, "Done calculating %dth fibonacci(%lu)\n", *(int*)req->data, GetCurrentThreadId());
}

int main()
{
	int data[FIB_UNTIL];
	uv_work_t req[FIB_UNTIL];
	int i;

	loop = uv_default_loop();
	for (i = 0; i < FIB_UNTIL; ++i)
	{
		data[i] = i;
		req[i].data = (void*)&data[i];
		uv_queue_work(loop, &req[i], fib, after_fib);
	}

	return uv_run(loop, UV_RUN_DEFAULT);
}