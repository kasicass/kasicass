#include <windows.h>
#include "iids.hpp"
#include "CoCatFactory.hpp"

ULONG g_lockCount = 0; // modify by IClassFactory::LockServer
ULONG g_objCount  = 0; // modified by ctor & dtor of any coclass in the server

extern "C" STDAPI DllGetClassObject(REFCLSID rclsid, REFIID riid, void** ppv)
{
	if (rclsid != CLSID_CoCat)
		return CLASS_E_CLASSNOTAVAILABLE;

	CoCatFactory *pFactory = new CoCatFactory;
	HRESULT hr = pFactory->QueryInterface(riid, ppv);
	if (FAILED(hr))
		delete pFactory;
	return hr;
}

extern "C" STDAPI DllCanUnloadNow(void)
{
	if (g_lockCount == 0 && g_objCount == 0)
		return S_OK;
	else
		return S_FALSE;
}

extern "C" BOOL WINAPI DllMain(_In_ HINSTANCE, _In_ DWORD, _In_ LPVOID)
{
	return TRUE;
}

