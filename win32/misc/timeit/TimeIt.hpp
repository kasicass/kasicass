#pragma once

class TimeIt
{
public:
	TimeIt();
	~TimeIt();

	double elapsed();

	void start();
	void stop();

private:
	double secondsPerCount_;

	__int64 startTime_;
	__int64 stopTime_;
};

