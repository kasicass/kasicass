#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <sstream>

std::wstring readFile(const std::wstring& filename)
{
	FILE *fp = _wfopen(filename.c_str(), L"rb");
	if (!fp) return std::wstring();

	fseek(fp, 0, SEEK_END);
	long sz = ftell(fp);

	fseek(fp, 0, SEEK_SET);
	wchar_t *buf = (wchar_t*)malloc(sz);
	fread(buf, sz, 1, fp);

	std::wstring text;
	text.assign(buf+1, sz/2-1);  // buf+1, skip BOM

	free(buf);
	fclose(fp);

	//OutputDebugString((filename + L"|toolchainAA|" + text).c_str());
	//if (CopyFile(filename.c_str(), L"D:\\cl.txt", TRUE) == FALSE)
	//{
	//	CopyFile(filename.c_str(), L"D:\\link.txt", FALSE);
	//}
	
	return text;
}

int wmain(int argc, wchar_t *argv[])
{
	std::wstringstream output;

	for (int i = 0; i < argc; ++i)
	{
		if (argv[i][0] == L'@')
		{
			output << readFile(argv[i] + 1);
		}
		else
		{
			output << argv[i];
			output << L" ";
		}
	}

	OutputDebugString(output.str().c_str());
	return 0;
}
