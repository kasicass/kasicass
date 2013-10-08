#pragma once

DECLARE_INTERFACE_(ICatView, IUnknown)
{
	STDMETHOD(Cry)() PURE;
	STDMETHOD(GetName)(BSTR* name) PURE;
};

DECLARE_INTERFACE_(ICatEdit, IUnknown)
{
	STDMETHOD(SetName)(BSTR name) PURE;
};

