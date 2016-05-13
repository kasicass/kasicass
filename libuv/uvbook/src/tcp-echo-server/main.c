#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <uv.h>

#if defined(_WIN32)
#if defined(DEBUG) || defined(_DEBUG)
#include <crtdbg.h>
#endif
#endif

#define DEFAULT_PORT 7000
#define DEFAULT_BACKLOG 128

uv_loop_t *loop;
struct sockaddr_in addr;

void alloc_buffer(uv_handle_t *handle, size_t suggested_size, uv_buf_t *buf)
{
	buf->base = (char*)malloc(suggested_size);
	buf->len  = suggested_size;
}

void echo_write(uv_write_t *req, int status)
{
	if (status)
		fprintf(stderr, "Write error %s\n", uv_strerror(status));
	free(req);
}

void echo_read(uv_stream_t *client, ssize_t nread, const uv_buf_t *buf)
{
	if (nread < 0)
	{
		if (nread != UV_EOF)
			fprintf(stderr, "Read error %s\n", uv_err_name(nread));
		uv_close((uv_handle_t*)client, NULL);
	}
	else if (nread > 0)
	{
		uv_write_t *req = (uv_write_t*)malloc(sizeof(uv_write_t));
		uv_buf_t wrbuf = uv_buf_init(buf->base, nread);
		uv_write(req, client, &wrbuf, 1, echo_write);
	}

	// 上面 wrbuf = uv_buf_init(buf->base, nread) 直接使用了 buf->base
	// 而这里立即就 free 了，是否有问题？
	// 不会！
	// <1> win32 下，uv_write() 会调用 WSASend()，buf 内容直接复制给 kernel
	// <2> unix 下，uv_write() 会复制一份 buf，然后丢入后台 queue，等待 thread 去 send
	if (buf->base)
		free(buf->base);
}

void on_new_connection(uv_stream_t *server, int status)
{
	uv_tcp_t *client;

	if (status < 0)
	{
		fprintf(stderr, "New connection error %s\n", uv_strerror(status));
		return;
	}

	client = (uv_tcp_t*)malloc(sizeof(uv_tcp_t));
	uv_tcp_init(loop, client);
	if (uv_accept(server, (uv_stream_t*)client) == 0)
		uv_read_start((uv_stream_t*)client, alloc_buffer, echo_read);
	else
		uv_close((uv_handle_t*)client, NULL);
}

int main()
{
	int r;
	uv_tcp_t server;

#if defined(_WIN32)
#if defined(DEBUG) || defined(_DEBUG)
    _CrtSetDbgFlag( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
#endif
#endif

	loop = uv_default_loop();

	uv_tcp_init(loop, &server);

	uv_ip4_addr("0.0.0.0", DEFAULT_PORT, &addr);

	uv_tcp_bind(&server, (const struct sockaddr*)&addr, 0);
	r = uv_listen((uv_stream_t*)&server, DEFAULT_BACKLOG, on_new_connection);
	if (r)
	{
		fprintf(stderr, "Listen error %s\n", uv_strerror(r));
		return 1;
	}

	return uv_run(loop, UV_RUN_DEFAULT);
}
