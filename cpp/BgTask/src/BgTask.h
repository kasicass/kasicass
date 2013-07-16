#ifndef BACKGROUND_TASK_H
#define BACKGROUND_TASK_H

#include <vector>
#include <memory>

class BgTask
{
public:
	BgTask() {}
	virtual ~BgTask() {}

	virtual void doTask() = 0;
	virtual void doFinish() = 0;
};
typedef std::shared_ptr<BgTask> BgTaskPtr;

// getTasks(), put the tasks to bg-thread per BgTaskManager::tick()
class BgTaskStrategy
{
public:
	static BgTaskStrategy *No;

public:
	virtual void addTask(BgTaskPtr task) = 0;
	virtual std::vector<BgTaskPtr> getTasks() = 0;
};

class BgThread;
class BgTaskManager
{
public:
	static BgTaskManager *instance();

public:
	BgTaskManager();
	~BgTaskManager();

	void init(unsigned int numOfThreads, BgTaskStrategy *strategy);
	void fini();

	void tick();
	void addTask(BgTask* task);

private:
	void addTaskInternal(BgTaskPtr task);
	void checkNewTask();
	void checkFinishTask();

private:
	unsigned int currThr_;
	std::vector<BgThread*> thrPool_;
	BgTaskStrategy *strategy_;
};

#endif
