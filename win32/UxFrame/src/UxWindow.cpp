#include "UxWindow.hpp"
#include "UxUtil.hpp"
#include "UxApp.hpp"

namespace Ux {

static LRESULT CALLBACK WndProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam);
Window::Window() : hWnd_(0)
{
	HINSTANCE hInst = App::instance().getHINSTANCE();
	WinClassMaker wcm(hInst, "UxWindow", WndProc);
	wcm.registerMe();

	WinMaker wm(hInst, "UxWindow");
	hWnd_ = wm.create("Hello UxWindow!", this);
}

Window::~Window()
{
}

void Window::show()
{
	::ShowWindow(hWnd_, SW_SHOWNORMAL);
	::UpdateWindow(hWnd_);
}

void Window::onDestroy()
{
}

WindowPtr createWindow()
{
	return std::make_shared<Window>();
}

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

	case WM_DESTROY:
		pWin->onDestroy();
		::PostQuitMessage(0);
		return 0;
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
