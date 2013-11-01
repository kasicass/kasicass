// kill a process by name

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>

int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		printf("usage: kill.exe <processName>\n");
		return 0;
	}

	HWND hWnd = FindWindow(NULL, argv[1]);
	if (hWnd == NULL)
	{
		printf("FindWindow() failed, ecode: %u\n", GetLastError());
		return 1;
	}

	DWORD dwProcessId;
	GetWindowThreadProcessId(hWnd, &dwProcessId);

	HANDLE hProcess = OpenProcess(PROCESS_TERMINATE, FALSE, dwProcessId);
	if (hProcess == NULL)
	{
		printf("OpenProcess() failed, ecode: %u\n", GetLastError());
		return 1;
	}

	TerminateProcess(hProcess, 0);
	CloseHandle(hProcess);
	return 0;
}

