#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <uv.h>

#define DEFAULT_PORT 7000

uv_loop_t *loop;
struct sockaddr_in addr;

void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf)
{
	buf->base = (char*)malloc(suggested_size);
	buf->len  = suggested_size;
}

void on_read(uv_stream_t *peer, ssize_t nread, const uv_buf_t *buf)
{
	if (nread < 0)
	{
		if (nread != UV_EOF)
		{
			fprintf(stderr, "Read error %s\n", uv_err_name(nread));
		}
		uv_close((uv_handle_t*)peer, NULL);
	}
	else if (nread > 0)
	{
		printf("Recv: %s\n", buf->base);  // ignore tcp-stream based here
		uv_close((uv_handle_t*)peer, NULL);
	}

	if (buf->base)
		free(buf->base);
}

void on_write(uv_write_t *write_req, int status)
{
	if (status)
	{
		fprintf(stderr, "Write error %s\n", uv_strerror(status));
	}

	printf("Hello send.\n");
	uv_read_start(write_req->handle, alloc_buffer, on_read);

	free(write_req);
}

void on_connect(uv_connect_t *connect_req, int status)
{
	char hello_string[] = "Hello!!";
	uv_write_t *write_req;
	uv_buf_t wrbuf;

	if (status < 0)
	{
		fprintf(stderr, "on_connect error %s\n", uv_strerror(status));
		return;
	}

	write_req = (uv_write_t*)malloc(sizeof(uv_write_t));
	wrbuf = uv_buf_init((char*)hello_string, sizeof(hello_string));
	uv_write(write_req, connect_req->handle, &wrbuf, 1, on_write);
}

int main()
{
	int r;
	uv_tcp_t client;
	uv_connect_t connect_req;

	loop = uv_default_loop();

	uv_tcp_init(loop, &client);
	uv_ip4_addr("127.0.0.1", DEFAULT_PORT, &addr);
	r = uv_tcp_connect(&connect_req, &client, (const struct sockaddr*)&addr, on_connect);
	if (r)
	{
		fprintf(stderr, "Connect error %s\n", uv_strerror(r));
		return 1;
	}

	return uv_run(loop, UV_RUN_DEFAULT);
}