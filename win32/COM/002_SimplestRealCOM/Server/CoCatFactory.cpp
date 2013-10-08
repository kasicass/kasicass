#include <windows.h>
#include "CoCatFactory.hpp"
#include "CoCat.hpp"

STDMETHODIMP CoCatFactory::QueryInterface(REFIID riid, void** pIFace)
{
	if (riid == IID_IUnknown)
	{
		*pIFace = (IUnknown*)this;
	}
	else if (riid == IID_IClassFactory)
	{
		*pIFace = (IClassFactory*)this;
	}

	if (*pIFace)
	{
		((IUnknown*)(*pIFace))->AddRef();
		return S_OK;
	}

	*pIFace = NULL;
	return E_NOINTERFACE;
}

STDMETHODIMP_(ULONG) CoCatFactory::AddRef()
{
	return ++refCount_;
}

STDMETHODIMP_(ULONG) CoCatFactory::Release()
{
	if (--refCount_ == 0)
	{
		delete this;
		return 0;
	}
	return refCount_;
}

STDMETHODIMP CreateInstance(LPUNKNOWN pUnk, REFIID riid, void** pIFace)
{
	// We do not support aggregation in this class object
	if (pUnk != NULL)
		return CLASS_E_NOAGGREGATION;

	CoCat* pCat = new CoCat;
	HRESULT hr = pCat->QueryInterface(riid, pIFace);
	if (FAILED(hr))
		delete pCat;
	return hr;
}

extern int g_lockCount;
STDMETHODIMP CoCatFactory::LockServer(BOOL fLock)
{
	if (fLock) ++g_lockCount;
	else --g_lockCount;
	return S_OK;
}

