#ifndef KTHREAD_H
#define KTHREAD_H

#include <Windows.h>

// simple thread wrapper
class KThread
{
public:
	KThread();
	virtual ~KThread();

	void run();
	void join();

private:
	virtual void workFunc() = 0;

private:
	static DWORD WINAPI threadFunc(LPVOID lpParameter);

	HANDLE hThread_;
};

#endif