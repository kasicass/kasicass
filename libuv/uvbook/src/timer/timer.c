#include <stdio.h>
#include <time.h>
#include <uv.h>

char* current_time()
{
	time_t now = time(NULL);
	return asctime(gmtime(&now));
}

int32_t counter = 0;
void timer_callback(uv_timer_t* handle)
{
	++counter;
	if (counter > 10)
	{
		uv_timer_stop(handle);
	}
	else
	{
		printf("counter = %d, %s", counter, current_time());
	}
}

int main()
{
	uv_timer_t timer;

	uv_timer_init(uv_default_loop(), &timer);
	uv_timer_start(&timer, timer_callback, 5000, 2000); // timeout = 5 sec, repeat = 2 sec

	printf("timer start, %s", current_time());
	uv_run(uv_default_loop(), UV_RUN_DEFAULT);

	return 0;
}