#pragma once

#include "interfaces.hpp"

class CoCat : public ICatView, public ICatEdit
{
public:
	CoCat();
	virtual ~CoCat();

	// IUnknown
	STDMETHODIMP QueryInterface(REFIID riid, void** pIFace);
	STDMETHODIMP_(ULONG) AddRef();
	STDMETHODIMP_(ULONG) Release();

	// ICatView
	STDMETHODIMP Cry();
	STDMETHODIMP GetName(BSTR* name);

	// ICatEdit
	STDMETHODIMP SetName(BSTR name);

private:
	BSTR name_;
	UINT refCount_;
};

