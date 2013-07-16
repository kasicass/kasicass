#include "KThread.h"
#include <assert.h>

KThread::KThread() : hThread_(NULL)
{
}

KThread::~KThread()
{
	assert(hThread_ == NULL);
}

DWORD WINAPI KThread::threadFunc(LPVOID lpParameter)
{
	KThread *thr = (KThread *)lpParameter;
	thr->workFunc();
	return 0;
}

void KThread::run()
{
	assert(hThread_ == NULL);
	hThread_ = ::CreateThread(NULL, 0, KThread::threadFunc, this, 0, NULL);
}

void KThread::join()
{
	if (hThread_)
	{
		::WaitForSingleObject(hThread_, INFINITE);
		::CloseHandle(hThread_);
		hThread_ = NULL;
	}
}
