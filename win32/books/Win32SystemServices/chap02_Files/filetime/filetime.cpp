#include <windows.h>
#include <stdio.h>

void ShowTime(const char* prefix, FILETIME t)
{
	FILETIME ft;
	SYSTEMTIME st;

	FileTimeToLocalFileTime(&t, &ft);
	FileTimeToSystemTime(&ft, &st);
	printf("%s%d-%02d-%02d %02d:%02d\n", prefix, st.wYear,
		st.wMinute, st.wDay, st.wHour, st.wMinute);
}

int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		printf("usage: filetime.exe <file>\n");
		return 0;
	}

	HANDLE hFile = CreateFile(argv[1], GENERIC_READ, 
		FILE_SHARE_WRITE, 0, OPEN_EXISTING, 0, 0);
	if (hFile == INVALID_HANDLE_VALUE)
	{
		fprintf(stderr, "open failed, ecode: %u\n", GetLastError());
		return 1;
	}

	FILETIME create, lastAccess, lastWrite;
	BOOL success = GetFileTime(hFile, &create, &lastAccess, &lastWrite);
	ShowTime("Create Time:      ", create);
	ShowTime("Last Access Time: ", lastAccess);
	ShowTime("Last Write Time:  ", lastWrite);

	CloseHandle(hFile);
	return 0;
}
