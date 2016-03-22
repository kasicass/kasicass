#include "chronoclock.hpp"

TickClock::TickClock() {
	// std::ratio<1, 1000> == milli seconds
	// num == 1, den == 1000
	using namespace std::chrono;
	freq_ = (double)high_resolution_clock::period::num / (double)high_resolution_clock::period::den;
}

TickClock::~TickClock() {
}

void TickClock::start() {
	using namespace std::chrono;
	t1_ = high_resolution_clock::now().time_since_epoch().count();
}

void TickClock::stop() {
	using namespace std::chrono;
	t2_ = high_resolution_clock::now().time_since_epoch().count();
}

double TickClock::elapsed() {
	return (double)(t2_ - t1_) * freq_;
}

