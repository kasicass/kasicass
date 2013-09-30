#include "UxTimer.hpp"
#include <unordered_map>

namespace Ux {

static std::tr1::unordered_map<ULONG_PTR, Timer*> g_timerMap;

Timer::Timer(float interval, std::function<void()> func) :
  handle_(0),
  interval_(interval),
  func_(func)
{
}

Timer::~Timer()
{
	stop();
}

void Timer::start()
{
	stop();

	UINT elapse = static_cast<UINT>(interval_ * 1000);
	handle_ = ::SetTimer(NULL, 0, elapse, timerProc);

	g_timerMap[handle_] = this;
}

void Timer::stop()
{
	if (handle_)
	{
		g_timerMap.erase(handle_);

		::KillTimer(NULL, handle_);
		handle_ = 0;
	}
}

VOID CALLBACK Timer::timerProc(HWND hwnd, UINT uMsg, UINT_PTR idEvent, DWORD dwTime)
{
	auto it = g_timerMap.find(idEvent);
	if (it != g_timerMap.end())
	{
		it->second->func_();
	}
}

TimerPtr createTimer(float interval, std::function<void()> func)
{
	return std::make_shared<Timer>(interval, func);
}

}