#include <windows.h>
#include <objbase.h>
#include <iostream>

#pragma comment(lib, "Ole32.lib")

int wmain(int argc, wchar_t *argv[])
{
	if (argc != 2)
	{
		std::wcout << L"usage:" << std::endl
               << L"  LookupProgID.exe <ProgID>" << std::endl;
		return 0;
	}

	HRESULT hr;
	hr = CoInitialize(NULL);
	if (FAILED(hr))
	{
		std::wcout << L"CoInitialize failed" << std::endl;
		return 1;
	}

	CLSID clsid;
	hr = CLSIDFromProgID(argv[1], &clsid);
	if (SUCCEEDED(hr))
	{
		LPOLESTR s;
		hr = StringFromCLSID(clsid, &s);
		if (SUCCEEDED(hr))
		{
			std::wcout << "CLSID: " << s  << std::endl;
			CoTaskMemFree(s);
		}
	}

	CoUninitialize();
	return 0;
}

