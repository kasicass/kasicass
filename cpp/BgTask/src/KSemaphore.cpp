#include "KSemaphore.h"

KSemaphore::KSemaphore()
{
	sema_ = ::CreateSemaphore(NULL, 0, 0x7FFFFFFF, NULL);
}

KSemaphore::~KSemaphore()
{
	::CloseHandle(sema_);
}

void KSemaphore::inc(long n)
{
	ReleaseSemaphore(sema_, n, NULL);
}

void KSemaphore::dec()
{
	::WaitForSingleObject(sema_, INFINITE);
}

bool KSemaphore::decTry()
{
	return ::WaitForSingleObject(sema_, 0) == WAIT_OBJECT_0;
}
