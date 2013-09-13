#include <windows.h>
#include <objbase.h>

#pragma comment(lib, "Ole32.lib")

int main(int argc, char *argv[])
{
	if (argc != 2)
	{
		printf("usage:\n  LookupProgID.exe <ProgID>\n");
		return 0;
	}

	HRESULT hr;
	hr = CoInitialize(NULL);
	if (FAILED(hr))
	{
		printf("CoInitialize failed\n");
		return 1;
	}

	CLSID clsid;
	hr = CLSIDFromProgID(argv[1], &clsid);
	if (SUCCEED(hr))
	{
		StringFromCLSID(&clsid, );
		CoTaskMemFree(
	}

	CoUninitialize();
	return 0;
}

