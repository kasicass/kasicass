#pragma once

#include <process.h>
#include <assert.h>

class OSThread
{
public:
	OSThread() : hThread_(NULL) {}
	virtual ~OSThread() { join(); }

	void run()
	{
		assert(hThread_ == NULL);
		hThread_ = HANDLE( _beginthreadex(NULL, 0, OSThread::threadFunc, this, 0, NULL) );
	}

	void join()
	{
		if (hThread_)
		{
			::WaitForSingleObject(hThread_, INFINITE);
			::CloseHandle(hThread_);
			hThread_ = NULL;
		}
	}

private:
	virtual void workFunc() = 0;

	static unsigned int __stdcall threadFunc(void* arg)
	{
		OSThread *thr = (OSThread *)arg;
		thr->workFunc();
		return 0;
	}

private:
	HANDLE hThread_;
};


//
// PThread Wrapper
//
class PWrapperThread : public OSThread {
public:
	PWrapperThread(void *(*start_routine) (void *), void *arg) : func_(start_routine), arg_(arg) {}
	virtual ~PWrapperThread() {}

private:
	virtual void workFunc() {
		func_(arg_);
	}

private:
	void *(*func_) (void *);
	void *arg_;
};

typedef PWrapperThread* pthread_t;
int pthread_create(pthread_t *thread, const void *attr, void *(*start_routine) (void *), void *arg) {
	pthread_t pid = new PWrapperThread(start_routine, arg);
	pid->run();
	*thread = pid;
	return 0; // pid memory is leaked.
}