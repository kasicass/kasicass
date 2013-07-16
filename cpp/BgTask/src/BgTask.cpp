#include "BgTask.h"
#include "KThread.h"
#include "KThreadQueue.h"
#include <assert.h>


#define QUIT_TASK		nullptr

//
// BgThread
//
class BgThread : public KThread
{
public:
	BgThread();
	virtual ~BgThread();

	void putTask(BgTaskPtr task);
	BgTaskPtr getFinishTask();

private:
	virtual void workFunc();

private:
	KThreadQueue<BgTask> in_;
	KThreadQueue<BgTask> out_;
};

BgThread::BgThread()
{
}

BgThread::~BgThread()
{
}

void BgThread::putTask(BgTaskPtr task)
{
	in_.put(task);
}

BgTaskPtr BgThread::getFinishTask()
{
	return out_.getTry();
}

void BgThread::workFunc()
{
	while (1)
	{
		BgTaskPtr task = in_.get();
		if (task == QUIT_TASK)
			break;

		task->doTask();
		out_.put(task);
	}
}


//
// BgTaskStrategy
//
BgTaskStrategy *BgTaskStrategy::No = NULL;


//
// BgTaskManager
//

BgTaskManager* BgTaskManager::instance()
{
	static BgTaskManager s_instance;
	return &s_instance;
}

BgTaskManager::BgTaskManager() : currThr_(0), strategy_(NULL)
{
}

BgTaskManager::~BgTaskManager()
{
}

void BgTaskManager::init(unsigned int numOfThreads, BgTaskStrategy *strategy)
{
	assert(thrPool_.size() == 0);
	assert(numOfThreads > 0);

	strategy_ = strategy;
	currThr_  = 0;
	for (unsigned int i = 0; i < numOfThreads; i++)
	{
		BgThread *thr = new BgThread();
		thr->run();
		thrPool_.push_back(thr);
	}
}

void BgTaskManager::fini()
{
	assert(thrPool_.size() > 0);

	for (unsigned int i = 0; i < thrPool_.size(); i++)
	{
		thrPool_[i]->putTask(QUIT_TASK);
	}

	for (unsigned int i = 0; i < thrPool_.size(); i++)
	{
		thrPool_[i]->join();
		delete thrPool_[i];
	}

	thrPool_.clear();
}

void BgTaskManager::addTask(BgTask* pTask)
{
	BgTaskPtr task(pTask);

	if (strategy_)
		strategy_->addTask(task);
	else
		this->addTaskInternal(task);
}

void BgTaskManager::addTaskInternal(BgTaskPtr task)
{
	assert(thrPool_.size() > 0);

	thrPool_[currThr_]->putTask(task);
	currThr_ = (currThr_ + 1) % thrPool_.size();
}

void BgTaskManager::checkNewTask()
{
	if (!strategy_)
		return;

	std::vector<BgTaskPtr> tasks = strategy_->getTasks();
	if (tasks.empty())
		return;

	for (unsigned int i = 0; i < tasks.size(); i++)
	{
		this->addTaskInternal(tasks[i]);
	}
}

void BgTaskManager::checkFinishTask()
{
	for (unsigned int i = 0; i < thrPool_.size(); i++)
	{
		while (1)
		{
			BgTaskPtr task = thrPool_[i]->getFinishTask();
			if (!task)
				break;

			task->doFinish();
		}
	}
}

void BgTaskManager::tick()
{
	this->checkNewTask();
	this->checkFinishTask();
}
