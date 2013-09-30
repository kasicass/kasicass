#pragma once

#include <Windows.h>
#include <functional>
#include <memory>

namespace Ux {

class Timer
{
public:
	Timer(float interval, std::function<void()> func);
	~Timer();

	void start();
	void stop();

private:
	static VOID CALLBACK timerProc(HWND hwnd, UINT uMsg, UINT_PTR idEvent, DWORD dwTime);

private:
	ULONG_PTR handle_;
	float interval_;
	std::function<void()> func_;
};
typedef std::shared_ptr<Timer> TimerPtr;

TimerPtr createTimer(float interval, std::function<void()> func);

}
