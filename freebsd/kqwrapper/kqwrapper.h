#ifndef SGE_KQUEUE_WRAPPER
#define SGE_KQUEUE_WRAPPER

// simple kqueue wrapper for server-side program
// 1. 整个程序只允许启动一个 kqueue
//
// Author: Kasicass
// Date  : 2008-12-11


//
// Constants
//

#define	KQW_BACKLOG		128			// backlog for listen fd

#define	MAX_KQ_EVENTS		(1024 * 2)		// 2000+ 还不够你每次返回么? 开玩笑
#define	MAX_EVENTS		(1024 * 20)		// 1024 * 20 * sizeof(struct _kqw_event) == 0.48 Mb
							// 如果 fd 泄露, 操作系统早挂了, 自己进程 crash 也就无所谓了

#define	KQW_TIMERFD_1		(MAX_EVENTS-1)		// little trick, 使用最后几个 slot
#define	KQW_TIMERFD_2		(MAX_EVENTS-2)		// timer event 就给 5 个即可
#define	KQW_TIMERFD_3		(MAX_EVENTS-3)
#define	KQW_TIMERFD_4		(MAX_EVENTS-4)
#define	KQW_TIMERFD_5		(MAX_EVENTS-5)



//
// TCP Socket Stuff
//

// backlog 作为常量, 改上面的宏定义即可
// fd - 成功
// -1 - 失败
int kqw_create_listen_socket(int port);




// 
// Event Stuff
//

typedef void (*kqw_ev_handler) (int fd, void *data);

// 初始化 kqueue
// -1 - 失败
// 0  - 成功
int kqw_init();

// listen event
int kqw_add_listen_event(int fd, kqw_ev_handler listen_cb, void *data);

// 添加 read/write event
// write event is ONESHOT
int kqw_add_read_event(int fd, kqw_ev_handler read_cb, void *data);
int kqw_add_write_event(int fd, kqw_ev_handler write_cb, void *data);

// 添加 timer event
int kqw_add_timer(int fd, int interval, kqw_ev_handler timer_cb, void *data);

// 可以 del listen/client_conn/timer 任何类型的 fd
// 因为 write event 是 ONESHOT 的, 因此其实这里只清理 listen/read/timer event
int kqw_del_event(int fd);

// 每次有事件触发, 则回调对应的 handler, 同时返回
//
// 标准 main loop
// while (1) {
//   pre do ...
//   if ( kqw_event_dispatch() == KQW_DISPATCH_FATAL )
//   {
//     ba .. ba la ...
//   }
//   post do...
// }
//
// -1 - FATAL ERROR
// 0  - 成功
int kqw_event_dispatch();

#endif /* SGE_KQUEUE_WRAPPER */

