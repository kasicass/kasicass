#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include "TimeIt.hpp"
#include <iostream>

int main()
{
	TimeIt timeit;

	timeit.start();
	Sleep(1500);
	timeit.stop();

	std::cout << timeit.elapsed() << std::endl;
	return 0;
}

