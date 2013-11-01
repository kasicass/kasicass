#include <windows.h>
#include <stdio.h>

int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		printf("usage: filepath.exe <file>\n");
		return 0;
	}

	char path[1024];
	char *address;
	DWORD pathSize = GetFullPathName(argv[1], 1024, path, &address);
	if (pathSize == 0)
	{
		printf("GetFullPathName failed, ecode: %u\n", GetLastError());
		return 1;
	}
	else if (pathSize >= 1024)
	{
		printf("buf is too small.\n");
		return 1;
	}

	printf("path: %s\n", path);
	if (address) printf("file: %s\n", address);

	return 0;
}

