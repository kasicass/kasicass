#include <windows.h>
#include <stdio.h>

int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		printf("usage: filesize.exe <file>\n");
		return 0;
	}

	HANDLE hFile = CreateFile(argv[1], GENERIC_READ, 
		FILE_SHARE_WRITE, 0, OPEN_EXISTING, 0, 0);
	if (hFile == INVALID_HANDLE_VALUE)
	{
		fprintf(stderr, "open failed, ecode: %u\n", GetLastError());
		return 1;
	}

	LARGE_INTEGER size;
	if (!GetFileSizeEx(hFile, &size))  // GetFileSize() is old
	{
		fprintf(stderr, "GetFileSize failed, ecode: %u\n", GetLastError());
		CloseHandle(hFile);
		return 2;
	}

	printf("File Size: %I64d bytes.\n", size);
	CloseHandle(hFile);
	return 0;
}

