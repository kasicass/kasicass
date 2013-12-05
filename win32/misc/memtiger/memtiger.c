#include <windows.h>
#include <stdio.h>
#include <assert.h>

#define ONE_MB_SIZE (1024*1024)

int main(int argc, char* argv[])
{
	void *p = 0;
	int maxSize = 0;
	int currSize = 0;

	if (argc != 2)
	{
		printf("usage: memtiger.exe <sizeInMb>\n");
		return 1;
	}

	maxSize = atoi(argv[1]);

	while (currSize < maxSize)
	{
		currSize++;
		p = malloc(ONE_MB_SIZE);
		assert(p);
		memset(p, 0, ONE_MB_SIZE);
		Sleep(10);
	}

	return 0;
}

