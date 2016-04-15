#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/event.h>

#include <netinet/in.h>
#include <arpa/inet.h>

#include <unistd.h>
#include <fcntl.h>
#include <strings.h>		// bzero
#include <assert.h>
#include <errno.h>

#include <stdio.h>		// debug

#include "kqwrapper.h"

//#define	KQW_PRINTF(x, ...)	printf(x, __VA_ARGS__)
#define	KQW_PRINTF(x, ...)


//
// TCP Socket Stuff
//

static int _kqw_create_tcp_socket()
{
	return socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
}

static int _kqw_setnonblock(int fd)
{
	int flags;

	flags = fcntl(fd, F_GETFL);
	if (flags < 0)
		return -1;

	flags |= O_NONBLOCK;
	if (fcntl(fd, F_SETFL, flags) < 0)
		return -1;

	return 0;
}

static int _kqw_bind_and_listen(int fd, int port)
{
	struct sockaddr_in in_addr;
	int reuseaddr_on = 1;

	// addr reuse
	if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (const char *)&reuseaddr_on, sizeof(reuseaddr_on)) == -1)
		return -1;

	// bind & listen
	bzero(&in_addr, sizeof(in_addr));
	in_addr.sin_family      = AF_INET;
	in_addr.sin_addr.s_addr = htonl(INADDR_ANY);
	in_addr.sin_port        = htons(port);

	if ( bind(fd, (struct sockaddr *) &in_addr, sizeof(in_addr)) == -1 )
		return -1;

	if ( listen(fd, KQW_BACKLOG) == -1 )
		return -1;

	// set non-block
	return _kqw_setnonblock(fd);
}

// -- 对外接口
int kqw_create_listen_socket(int port)
{
	int listenfd;

	listenfd = _kqw_create_tcp_socket();
	if ( listenfd == -1 )
		return -1;

	if ( _kqw_bind_and_listen(listenfd, port) == -1 )
		return -1;

	return listenfd;
}



// 
// Event Stuff
//

// event type
#define	KQW_EVT_FREE		0		// event slot empty
#define	KQW_EVT_LISTEN		1
#define	KQW_EVT_CLIENT_CONN	2
#define	KQW_EVT_TIMER		3


struct _kqw_event {
	kqw_ev_handler read_callback;
	kqw_ev_handler write_callback;		// only for KQW_EVT_CLIENT_CONN

	int type;
};

static int kq;
static struct _kqw_event ev_list[MAX_EVENTS];
static struct kevent kq_evlist[MAX_KQ_EVENTS];

// -- 对外接口
int kqw_init()
{
	if ( kq > 0 )
		return 0;

	kq = kqueue();
	if ( kq != -1 )
		return 0;

	bzero(&ev_list[0], sizeof(ev_list));
	return -1;
}

int kqw_add_listen_event(int fd, kqw_ev_handler listen_cb, void *data)
{
	struct kevent kev;

	assert( ev_list[fd].type == KQW_EVT_FREE );

	EV_SET(&kev, fd, EVFILT_READ, EV_ADD, 0, KQW_BACKLOG, data);
	if ( kevent(kq, &kev, 1, NULL, 0, NULL) == -1 )
		return -1;

	ev_list[fd].type = KQW_EVT_LISTEN;
	ev_list[fd].read_callback = listen_cb;
	KQW_PRINTF("[kqw_add_listen_event] fd = %d\n", fd);
	return 0;
}

int kqw_add_read_event(int fd, kqw_ev_handler read_cb, void *data)
{
	struct kevent kev;

	assert( ev_list[fd].type == KQW_EVT_FREE );

	EV_SET(&kev, fd, EVFILT_READ, EV_ADD, 0, 0, data);
	if ( kevent(kq, &kev, 1, NULL, 0, NULL) == -1 )
		return -1;

	ev_list[fd].type = KQW_EVT_CLIENT_CONN;
	ev_list[fd].read_callback = read_cb;
	KQW_PRINTF("[kqw_add_read_event] fd = %d\n", fd);
	return 0;
}

int kqw_add_write_event(int fd, kqw_ev_handler write_cb, void *data)
{
	struct kevent kev;

	assert( ev_list[fd].type == KQW_EVT_CLIENT_CONN );

	EV_SET(&kev, fd, EVFILT_WRITE, EV_ADD|EV_ONESHOT, 0, 0, data);
	if ( kevent(kq, &kev, 1, NULL, 0, NULL) == -1 )
		return -1;

	ev_list[fd].write_callback = write_cb;
	KQW_PRINTF("[kqw_add_write_event] fd = %d\n", fd);
	return 0;
}

int kqw_add_timer(int fd, int interval, kqw_ev_handler timer_cb, void *data)
{
	struct kevent kev;

	assert( ev_list[fd].type == KQW_EVT_FREE );

	EV_SET(&kev, fd, EVFILT_TIMER, EV_ADD, 0, interval, data);
	if ( kevent(kq, &kev, 1, NULL, 0, NULL) == -1 )
		return -1;

	ev_list[fd].type = KQW_EVT_TIMER;
	ev_list[fd].read_callback = timer_cb;
	KQW_PRINTF("[kqw_add_timer] fd = %d\n", fd);
	return 0;
}

int kqw_del_event(int fd)
{
	struct kevent kev;

	assert( ev_list[fd].type != KQW_EVT_FREE );

	ev_list[fd].type = KQW_EVT_FREE;
	ev_list[fd].read_callback = NULL;

	KQW_PRINTF("[kqw_del_event] fd = %d\n", fd);

	EV_SET(&kev, fd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
	return kevent(kq, &kev, 1, NULL, 0, NULL);
}

int kqw_event_dispatch()
{
	int i, n, fd, filter;
	struct _kqw_event *ev;
	void *data;

	n = kevent(kq, NULL, 0, &kq_evlist[0], MAX_KQ_EVENTS, NULL);
	if ( n < 0 && n != EINTR )
		return -1;

	// boost events
	for ( i = 0; i < n; i++ )
	{
		fd   = kq_evlist[i].ident;
		data = kq_evlist[i].udata;
		ev   = &ev_list[fd];

		// KQW_PRINTF("kq fd = %d\n", fd);		// debug
		if ( ev->type == KQW_EVT_FREE )
			continue;

		if ( ev->type == KQW_EVT_CLIENT_CONN )
		{
			filter = kq_evlist[i].filter;
			if ( filter == EVFILT_READ && ev->read_callback )
			{
				ev->read_callback(fd, data);
			}
			else if ( filter == EVFILT_WRITE && ev->write_callback )
			{
				ev->write_callback(fd, data);
	
				// must be oneshot
				ev->write_callback = NULL;
			}
		}
		else
		{
			ev->read_callback(fd, data);
		}
	}

	return 0;
}

