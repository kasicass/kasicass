#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include "TimeIt.hpp"

TimeIt::TimeIt() :
	secondsPerCount_(0.0),
	startTime_(0),
	stopTime_(0)
{
	__int64 countsPerSec;
	QueryPerformanceFrequency((LARGE_INTEGER*)&countsPerSec);
	secondsPerCount_ = 1.0 / (double)countsPerSec;
}

TimeIt::~TimeIt()
{
}

double TimeIt::elapsed()
{
	return (stopTime_ - startTime_)*secondsPerCount_;
}

void TimeIt::start()
{
	QueryPerformanceCounter((LARGE_INTEGER*)&startTime_);
	stopTime_ = 0;
}

void TimeIt::stop()
{
	QueryPerformanceCounter((LARGE_INTEGER*)&stopTime_);
}

