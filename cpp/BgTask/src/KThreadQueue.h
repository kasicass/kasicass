#ifndef KTHREAD_QUEUE_H
#define KTHREAD_QUEUE_H

#include "KMutex.h"
#include "KSemaphore.h"
#include <deque>
#include <memory>

template <typename T>
class KThreadQueue
{
public:
	KThreadQueue();
	~KThreadQueue();

	void put(std::shared_ptr<T> data);
	std::shared_ptr<T> get();
	std::shared_ptr<T> getTry();

private:
	KMutex mutex_;
	KSemaphore signal_;
	std::deque<std::shared_ptr<T> > queue_;
};

template <typename T>
KThreadQueue<T>::KThreadQueue()
{
}

template <typename T>
KThreadQueue<T>::~KThreadQueue()
{
}

template <typename T>
void KThreadQueue<T>::put(std::shared_ptr<T> data)
{
	mutex_.lock();
	queue_.push_back(data);
	mutex_.unlock();

	signal_.inc(1);
}

template <typename T>
std::shared_ptr<T> KThreadQueue<T>::get()
{
	signal_.dec();

	mutex_.lock();
	std::shared_ptr<T> data = queue_.front();
	queue_.pop_front();
	mutex_.unlock();

	return data;
}

template <typename T>
std::shared_ptr<T> KThreadQueue<T>::getTry()
{
	if (!signal_.decTry())
		return NULL;

	mutex_.lock();
	std::shared_ptr<T> data = queue_.front();
	queue_.pop_front();
	mutex_.unlock();

	return data;
}

#endif