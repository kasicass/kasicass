#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

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

void my_timer(int fd, void *data)
{
	int n = (int)data;
	printf("timer n = %d\n", n);
}

void my_read(int fd, void *data)
{
	int n;
	char buf[80];

	n = read(fd, buf, sizeof(buf)-1);
	if ( n < 0 )
	{
		if ( errno == EINTR ) return;

		puts("read err");
		kqw_del_event(fd);
	}
	else if ( n == 0 )
	{
		printf("conn: %d, close\n", fd);
		close(fd);
		kqw_del_event(fd);
	}
	else
	{
		buf[n] = '\0';
		printf("recv: %d, %s\n", fd, buf);
	}	
}

void my_listen(int listenfd, void *data)
{
	int connfd;

	connfd = accept(listenfd, NULL, NULL);
	kqw_add_read_event(connfd, my_read, NULL);
}

// -----------------------

void my_write(int fd, void *data)
{
	int n;
	n = write(fd, "GET /index.html\r\n", sizeof("GET /index.html\r\n"));
	printf("write = %d\n", n);
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


int main()
{
	int listenfd, n;
	int sockfd;
        struct sockaddr_in addr;

	kqw_init();

//	kqw_add_timer(KQW_TIMERFD_1, 1000, my_timer, (void *)1);
//	kqw_add_timer(KQW_TIMERFD_2, 2000, my_timer, (void *)2);

/*
	listenfd = kqw_create_listen_socket(7788);
	if ( listenfd == -1 )
	{
		puts("create listen fd err");
		exit(1);
	}

	n = kqw_add_listen_event(listenfd, my_listen, NULL);
	printf("listenfd = %d, r = %d\n", listenfd, n);
*/

	sockfd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	printf("sockfd = %d\n", sockfd);

	_kqw_setnonblock(sockfd);

	kqw_add_read_event(sockfd, my_read, NULL);

       	// connect 
	bzero(&addr, sizeof(addr));
	addr.sin_family      = AF_INET;
	addr.sin_addr.s_addr = inet_addr("61.135.253.16");
	addr.sin_port        = htons(80);

	if ( connect(sockfd, (struct sockaddr *)&addr, sizeof(addr)) == -1 )
		perror("connect()");

	kqw_add_write_event(sockfd, my_write, NULL);
	
	while (1)
	{
		kqw_event_dispatch();
	}
}

