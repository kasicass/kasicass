#include <windows.h>
#include "iids.hpp"
#include "CoCatFactory.hpp"

STDAPI DllGetClassObject(REFCLSID rclsid, REFIID riid, void** ppv)
{
	if (rclsid != CLSID_CoCat)
		return CLASS_E_CLASSNOTAVAILABLE;

	CoCatFactory *pFactory = new CoCatFactory;
	HRESULT hr = pFactory->QueryInterface(riid, ppv);
	if (FAILED(hr))
		delete pFactory;
	return hr;
}

