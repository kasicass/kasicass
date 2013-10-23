#pragma once

template <class T, COLORREF BrushColor>
class MyPaintBackground
{
public:
	MyPaintBackground() { brush_ = ::CreateSolidBrush(BrushColor); }
	~MyPaintBackground() { ::DeleteObject(brush_); }

	BEGIN_MSG_MAP(MyPaintBackground)
		MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBackground)
	END_MSG_MAP()

	LRESULT OnEraseBackground(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		T* pT = static_cast<T*>(this);
		HDC dc = (HDC) wParam;
		RECT rcClient;
		pT->GetClientRect(&rcClient);
		::FillRect(dc, &rcClient, brush_);
		return 1; // we painted the background
	}

protected:
	HBRUSH brush_;
};

class MyWindow : public CWindowImpl<MyWindow, CWindow, CFrameWinTraits>,
                 public MyPaintBackground<MyWindow, RGB(0,0,255)>
{
public:
	DECLARE_WND_CLASS(_T("My Window Class"));

	typedef MyPaintBackground<MyWindow, RGB(0,0,255)> MyPaintBackgroundBase;
	BEGIN_MSG_MAP(MyWindow)
		MESSAGE_HANDLER(WM_CLOSE, OnClose)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		CHAIN_MSG_MAP(MyPaintBackgroundBase)
	END_MSG_MAP()

	LRESULT OnClose(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		DestroyWindow();
		return 0;
	}

	LRESULT OnDestroy(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		PostQuitMessage(0);
		return 0;
	}
};

