#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <psapi.h>
#include <stdio.h>

void printMemUsage(const char* text)
{
	PROCESS_MEMORY_COUNTERS pmc;

	ZeroMemory(&pmc, sizeof(PROCESS_MEMORY_COUNTERS));
	pmc.cb = sizeof(pmc);
	
	if (GetProcessMemoryInfo(GetCurrentProcess(), &pmc, sizeof(pmc)))
	{
		printf("[%s] WorkingSet: %u bytes, Committed: %u bytes\n", text, pmc.WorkingSetSize, pmc.PagefileUsage);
	}
}

int main(int argc, char* argv[])
{
	int i;
	int count;
	CRITICAL_SECTION *cs;

	if (argc != 2)
	{
		printf("usage: csmemtest.exe <count>\n");
		return 0;
	}

	count = atoi(argv[1]);

	printMemUsage("before alloc CS");

	cs = (CRITICAL_SECTION*) malloc(count * sizeof(CRITICAL_SECTION));

	printMemUsage("before init CS");

	for (i = 0; i < count; i++)
	{
		InitializeCriticalSection(&cs[i]);
		EnterCriticalSection(&cs[i]);
	}

	printMemUsage("after init CS");

	for (i = 0; i < count; i++)
	{
		LeaveCriticalSection(&cs[i]);
	}

	printMemUsage("before delete CS");

	for (i = 0; i < count; i++)
	{
		DeleteCriticalSection(&cs[i]);
	}

	printMemUsage("before dealloc CS");

	free(cs);
	cs = NULL;

	printMemUsage("after dealloc CS");

	return 0;
}

