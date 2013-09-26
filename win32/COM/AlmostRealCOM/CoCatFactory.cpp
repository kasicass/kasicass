#include <windows.h>
#include "CoCat.hpp"

HRESULT CatFactory(void** pIFace)
{
	HRESULT hr;
	LPUNKNOWN pUnk = NULL;
	CoCat *pCat = new CoCat();
	hr = pCat->QueryInterface(IID_IUnknown, (void**)&pUnk);
	if (SUCCEEDED(hr))
	{
		*pIFace = pUnk;
		return S_OK;
	}
	else
	{
		delete pCat;
		return E_FAIL;
	}
}

