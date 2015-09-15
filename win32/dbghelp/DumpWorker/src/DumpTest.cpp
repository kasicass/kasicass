#include "DumpWorker.hpp"
#include <stdio.h>

int main()
{
	DumpWorker::Init("mytest.dmp", DumpWorker::MINI_DUMP);

	int *p = 0;
	*p = 10;

	return 0;
}
