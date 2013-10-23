#include <windows.h>
#include "iids.hpp"
#include "CoCat.hpp"
#include <iostream>

extern ULONG g_objCount;
CoCat::CoCat() : refCount_(0)
{
	g_objCount++;
	name_ = SysAllocString(L"Default Name");
}

CoCat::~CoCat()
{
	g_objCount--;
	if (name_)
		SysFreeString(name_);
}

STDMETHODIMP_(ULONG) CoCat::AddRef()
{
	return ++refCount_;
}

STDMETHODIMP_(ULONG) CoCat::Release()
{
	if (--refCount_ == 0)
	{
		delete this;
		return 0;
	}
	else
	{
		return refCount_;
	}
}

STDMETHODIMP CoCat::QueryInterface(REFIID riid, void** pIFace)
{
	if (riid == IID_IUnknown)
	{
		*pIFace = (IUnknown*)(ICatView*)this;
	}
	else if (riid == IID_ICatView)
	{
		*pIFace = (ICatView*)this;
	}
	else if (riid == IID_ICatEdit)
	{
		*pIFace = (ICatEdit*)this;
	}
	else
	{
		*pIFace = NULL;
		return E_NOINTERFACE;
	}

	((IUnknown*)(*pIFace))->AddRef();
	return S_OK;
}

STDMETHODIMP CoCat::Cry()
{
	std::cout << "Cat Cry!" << std::endl;
	return S_OK;
}

STDMETHODIMP CoCat::GetName(BSTR* name)
{
	*name = SysAllocString(name_);
	return S_OK;
}

STDMETHODIMP CoCat::SetName(BSTR name)
{
	SysReAllocString(&name_, name);
	return S_OK;
}

