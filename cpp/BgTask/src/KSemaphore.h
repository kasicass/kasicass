#ifndef KTHREAD_SEMAPHORE_H
#define KTHREAD_SEMAPHORE_H

#include <Windows.h>

class KSemaphore
{
public:
	KSemaphore();
	~KSemaphore();

	void inc(long n);
	void dec();
	bool decTry();

private:
	HANDLE sema_;
};

#endif