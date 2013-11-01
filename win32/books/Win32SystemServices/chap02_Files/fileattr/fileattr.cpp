#include <windows.h>
#include <stdio.h>

void ShowAttributes(DWORD attr)
{
	if (attr & FILE_ATTRIBUTE_ARCHIVE)
		printf("  archive\n");
	if (attr & FILE_ATTRIBUTE_DIRECTORY)
		printf("  directory\n");
	if (attr & FILE_ATTRIBUTE_HIDDEN)
		printf("  hidden\n");
	if (attr & FILE_ATTRIBUTE_NORMAL)
		printf("  normal\n");
	if (attr & FILE_ATTRIBUTE_READONLY)
		printf("  read only\n");
	if (attr & FILE_ATTRIBUTE_SYSTEM)
		printf("  system\n");
	if (attr & FILE_ATTRIBUTE_TEMPORARY)
		printf("  temporary\n");
}

int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		printf("usage: fileattr <file>\n");
		return 0;
	}

	DWORD attr = GetFileAttributes(argv[1]);
	if (attr == INVALID_FILE_ATTRIBUTES)
	{
		printf("failed: %u\n", GetLastError());
		return 1;
	}

	ShowAttributes(attr);
	return 0;
}

