#include "DumpWorker.hpp"
#include <stdlib.h>

int main()
{
	DumpWorker::Init("mytest.dmp", DumpWorker::FULL_DUMP);

	int *a = (int*) malloc(sizeof(int));
	*a = 20;

	int *p = 0;
	*p = 10;  // crash me

	free(a);
	return 0;
}
