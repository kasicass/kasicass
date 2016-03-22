#pragma once

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

class TickClock {
public:
	TickClock();
	~TickClock();

	void start();
	void stop();
	double elapsed();

private:
	LARGE_INTEGER t1_;
	LARGE_INTEGER t2_;

private:
	static LARGE_INTEGER s_freq;
};

