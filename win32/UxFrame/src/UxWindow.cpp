#include "UxWindow.hpp"
#include "UxGlobal.hpp"
#include "UxUtil.hpp"
#include "UxDC.hpp"

namespace Ux {

static LRESULT CALLBACK WndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam);
Window::Window(UINT id) : hWnd_(0)
{
	HINSTANCE hInst = Global::getHINSTANCE();

	WinClassMaker wcm(hInst, "UxWindow", WndProc);
	wcm.registerMe();

	WinMaker wm(hInst, "UxWindow");
	hWnd_ = wm.create("Hello UxWindow!", this);

	bgImage_.load(id);
}

Window::~Window()
{
}

void Window::show()
{
	draw();

	::ShowWindow(hWnd_, SW_SHOWNORMAL);
	::UpdateWindow(hWnd_);

	MSG msg;
	ZeroMemory(&msg, sizeof(msg));
	while (::GetMessage(&msg, NULL, 0, 0))
	{
		::TranslateMessage(&msg);
		::DispatchMessage(&msg);
	}
}

void Window::draw()
{
	WndDC dc(hWnd_);
	MemoryDC memdc(dc, bgImage_.width(), bgImage_.height());

	// draw window && children
	{
		int x = ::GetSystemMetrics(SM_CXBORDER);
		int y = ::GetSystemMetrics(SM_CYCAPTION);

		Gdiplus::Graphics g(memdc);
		g.DrawImage(bgImage_, 0, 0);

		for (auto it = children_.cbegin(); it != children_.cend(); ++it)
		{
			if (!(*it)->visible()) continue;
			(*it)->onDraw(g, x, y);
		}
	}

	// blend to system
	BLENDFUNCTION blend;
	ZeroMemory(&blend, sizeof(blend));
	blend.BlendOp = AC_SRC_OVER;
	blend.AlphaFormat = AC_SRC_ALPHA;
	blend.SourceConstantAlpha = 255;

	SIZE size;
	size.cx = bgImage_.width();
	size.cy = bgImage_.height();
	POINT src = {0, 0};

	// use alpha blend
	UpdateLayeredWindow(hWnd_, dc, NULL, &size, memdc, &src, RGB(255, 0, 255), &blend, ULW_ALPHA);

	// use color key
	//UpdateLayeredWindow(hWnd_, dc, NULL, &size, memdc, &src, RGB(255, 0, 255), &blend, ULW_COLORKEY);
}

void Window::addComponent(ComponentPtr child)
{
	child->createMe(hWnd_);
	children_.push_back(child);
}

#define WM_UXWINDOW_REDRAW (WM_USER+1)
static LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	static Window *pWin = nullptr;
	switch (message)
	{
	case WM_NCCREATE:
		{
		CREATESTRUCT *cs = reinterpret_cast<CREATESTRUCT *>(lParam);
		pWin = reinterpret_cast<Window *>(cs->lpCreateParams);
		}
		break;

	case WM_UXWINDOW_REDRAW:
		pWin->draw();
		return 0;

	case WM_DESTROY:
		::PostQuitMessage(0);
		return 0;

	case WM_NCHITTEST:
		return HTCAPTION;

	/*
	case WM_KEYDOWN:
		{
		KeyPressWin32 kPress(wParam);
		if (pWin->GetController().get())
			pWin->GetController()->OnKeyDown(kPress.GetKey());
		}
		return 0;

	case WM_LBUTTONDOWN:
		{
		POINTS p = MAKEPOINTS(lParam);
		KeyStateWin32 kState(wParam);
		if (pWin->GetController().get())
			pWin->GetController()->OnLButtonDown(p.x, p.y, kState);
		}
		return 0;

	case WM_MOUSEMOVE:
		{
		POINTS p = MAKEPOINTS(lParam);
		KeyStateWin32 kState(wParam);
		if (pWin->GetController().get())
			pWin->GetController()->OnMouseMove(p.x, p.y, kState);
		}
		return 0;
	*/
	}

	return DefWindowProc (hWnd, message, wParam, lParam) ;
}

}
