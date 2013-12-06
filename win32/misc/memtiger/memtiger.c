#include <windows.h>
#include <stdio.h>
#include <assert.h>

#define ONE_MB_SIZE (1024*1024)

int main(int argc, char* argv[])
{
	void *p = 0;
	int maxSize = 0;
	int currSize = 0;

	if (argc > 2)
	{
		printf("usage: memtiger.exe [sizeInMb]\n");
		return 1;
	}

	if (argc == 2)
	{
		maxSize = atoi(argv[1]);
	}
	else
	{
		maxSize = 8 * 1024;  // 8G
	}

	while (currSize < maxSize)
	{
		p = malloc(ONE_MB_SIZE);
		if (p == NULL)
		{
			break;
		}
		currSize++;
		memset(p, 0, ONE_MB_SIZE);
		Sleep(10);
	}

	printf("allocated %dMb\n", currSize);
	return 0;
}

