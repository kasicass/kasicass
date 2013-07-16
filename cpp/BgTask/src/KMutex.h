#ifndef KTHREAD_MUTEX_H
#define KTHREAD_MUTEX_H

#include <Windows.h>

class KMutex
{
public:
	KMutex();
	~KMutex();

	void lock();
	void unlock();

private:
	CRITICAL_SECTION cs_;
};

#endif