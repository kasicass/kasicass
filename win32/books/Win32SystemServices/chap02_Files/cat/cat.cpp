#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <iostream>

int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		std::cout << "usage: cat.exe <txtfile>" << std::endl;
		return 1;
	}

	HANDLE hFile = CreateFile(argv[1], GENERIC_READ, 0, 0, OPEN_EXISTING, 0, 0);
	if (hFile == INVALID_HANDLE_VALUE)
	{
		std::cout << "can't open \"" << argv[1] << "\", ecode:" << GetLastError() << std::endl;
		return 2;
	}

	BOOL success;
	char s[16];
	DWORD numReaded;
	do
	{
		success = ReadFile(hFile, s, 1, &numReaded, 0);
		s[numReaded] = 0;
		std::cout << s;
	} while (numReaded > 0 && success);

	CloseHandle(hFile);
	return 0;
}

