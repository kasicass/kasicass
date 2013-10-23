#pragma once

class CoCatFactory : public IClassFactory
{
public:
	// IUnknown methods
	STDMETHODIMP QueryInterface(REFIID riid, void** pIFace);
	STDMETHODIMP_(ULONG) AddRef();
	STDMETHODIMP_(ULONG) Release();

	// IClassFactory methods
	STDMETHODIMP CreateInstance(LPUNKNOWN pUnk, REFIID riid, void** pIFace);
	STDMETHODIMP LockServer(BOOL fLock);

private:
	ULONG refCount_;
};

