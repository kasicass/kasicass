#define WIN32_LEAN_AND_MEAN
#include <windows.h>

int main()
{
	HANDLE hHeap;
	ULONG heapType;

	hHeap = HeapCreate(0, 0, 0);
	HeapQueryInformation(hHeap, HeapCompatibilityInformation, &heapType,
		sizeof(heapType), NULL);
	HeapDestroy(hHeap);
	printf("HeapCreate() type: %u\n", heapType);

	HeapQueryInformation(GetProcessHeap(), HeapCompatibilityInformation,
		&heapType, sizeof(heapType), NULL);
	printf("GetProcessHeap() type: %u\n", heapType);

	return 0;
}

