#include "BgTask.h"
#include <Windows.h>
#include <stdio.h>

class MyBaseTask : public BgTask
{
public:
	enum Priority {
		kPriHigh = 0,
		kPriNormal,
		kPriLow,
		kPriNum
	};

	MyBaseTask(Priority prio) : prio_(prio) {}

	Priority priority() { return prio_; }

	virtual void doTask() = 0;
	virtual void doFinish() = 0;

private:
	Priority prio_;
};

class HighTask : public MyBaseTask
{
public:
	HighTask() : MyBaseTask(kPriHigh) { printf("#%d HighTask::ctor\n", ::GetCurrentThreadId()); }
	~HighTask() { printf("#%d HighTask::dtor\n", ::GetCurrentThreadId()); }

	virtual void doTask() { printf("#%d HighTask::doTask\n", ::GetCurrentThreadId()); }
	virtual void doFinish() { printf("#%d HighTask::doFinish\n", ::GetCurrentThreadId()); }
};

class NormalTask : public MyBaseTask
{
public:
	NormalTask() : MyBaseTask(kPriNormal) { printf("#%d NormalTask::ctor\n", ::GetCurrentThreadId()); }
	~NormalTask() { printf("#%d NormalTask::dtor\n", ::GetCurrentThreadId()); }

	virtual void doTask() { printf("#%d NormalTask::doTask\n", ::GetCurrentThreadId()); }
	virtual void doFinish() { printf("#%d NormalTask::doFinish\n", ::GetCurrentThreadId()); }
};

class LowTask : public MyBaseTask
{
public:
	LowTask() : MyBaseTask(kPriLow) { printf("#%d LowTask::ctor\n", ::GetCurrentThreadId()); }
	~LowTask() { printf("#%d LowTask::dtor\n", ::GetCurrentThreadId()); }

	virtual void doTask() { printf("#%d LowTask::doTask\n", ::GetCurrentThreadId()); }
	virtual void doFinish() { printf("#%d LowTask::doFinish\n", ::GetCurrentThreadId()); }
};

class MyStrategy : public BgTaskStrategy
{
public:
	virtual void addTask(BgTaskPtr task)
	{
		MyBaseTask *btask = reinterpret_cast<MyBaseTask *>(task.get());
		prioSlots_[btask->priority()].push_back(task);
	}

	virtual std::vector<BgTaskPtr> getTasks()
	{
		std::vector<BgTaskPtr> ret;
		if (prioSlots_[MyBaseTask::kPriHigh].size() > 0)
		{
			ret = prioSlots_[MyBaseTask::kPriHigh];
			prioSlots_[MyBaseTask::kPriHigh].clear();
			return ret;
		}

		if (prioSlots_[MyBaseTask::kPriNormal].size() > 0)
		{
			ret = prioSlots_[MyBaseTask::kPriNormal];
			prioSlots_[MyBaseTask::kPriNormal].clear();
			return ret;
		}

		if (prioSlots_[MyBaseTask::kPriLow].size() > 0)
		{
			ret = prioSlots_[MyBaseTask::kPriLow];
			prioSlots_[MyBaseTask::kPriLow].clear();
			return ret;
		}

		return ret;
	}

private:
	std::vector<BgTaskPtr> prioSlots_[MyBaseTask::kPriNum];
};

int main()
{
	puts("mgr::init");
	// BgTaskManager::instance()->init(3, TaskStrategy::No);
	BgTaskManager::instance()->init(1, new MyStrategy);

	BgTaskManager::instance()->addTask(new HighTask);
	BgTaskManager::instance()->addTask(new LowTask);
	BgTaskManager::instance()->addTask(new NormalTask);
	BgTaskManager::instance()->addTask(new HighTask);

	int cnt = 0;
	while (1)
	{
		cnt++;
		if (cnt > 10)
			break;

		BgTaskManager::instance()->tick();
		Sleep(10000);
	}
	
	puts("mgr::fini");
	BgTaskManager::instance()->fini();
	return 0;
}
