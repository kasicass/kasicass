#pragma once

#include <chrono>

class TickClock {
public:
	TickClock();
	~TickClock();

	void start();
	void stop();
	double elapsed();

private:
	std::chrono::high_resolution_clock::rep t1_;
	std::chrono::high_resolution_clock::rep t2_;

	double freq_;
};

