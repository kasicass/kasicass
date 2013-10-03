#include <windows.h>
#include <stdio.h>

typedef void (WINAPI *FUNC)();
int main()
{
	HMODULE hDLL = LoadLibrary("PrintMe.dll");
	if (!hDLL)
	{
		printf("LoadLibrary failed!\n");
		return 0;
	}

	FUNC fn = (FUNC) GetProcAddress(hDLL, "printMe");
	if (!fn)
	{
		printf("GetProcAddress failed!\n");
		return 0;
	}

	fn();
	return 0;
}

