#include <windows.h>
#include <stdio.h>
#include "iids.hpp"
#include "interfaces.hpp"
#include "CoCatFactory.hpp"

int main()
{
	IUnknown* pUnk;
	HRESULT hr;
	hr = CatFactory((void**) &pUnk);
	if (FAILED(hr))
	{
		printf("CatFactory failed!\n");
		return 0;
	}

	ICatEdit *pEdit;
	hr = pUnk->QueryInterface(IID_ICatEdit, (void**)&pEdit);
	if (SUCCEEDED(hr))
	{
		BSTR name = SysAllocString(OLESTR("Phay"));
		pEdit->SetName(name);
		SysFreeString(name);
		pEdit->Release();
	}

	ICatView *pView;
	hr = pUnk->QueryInterface(IID_ICatView, (void**)&pView);
	if (SUCCEEDED(hr))
	{
		pView->Cry();

		BSTR name;
		pView->GetName(&name);
		char buf[256];
		wcstombs(buf, name, 256);
		printf("CatName: %s\n", buf);
		SysFreeString(name);

		pView->Release();
	}

	pUnk->Release();
	return 0;
}

