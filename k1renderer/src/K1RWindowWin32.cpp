#include "K1RWindowWin32.h"
#include "K1RControllerWin32.h"
#include "K1RPropList.h"

namespace K1R {

static const LPCTSTR MY_WNDCLS_NAME = TEXT("K1R_WINDOW_WIN32_CLASS");

WindowWin32::WindowWin32(PropList& ParamList) : m_hWnd(NULL)
{
	m_iWidth  = ParamList.GetInteger("winWidth", 800);
	m_iHeight = ParamList.GetInteger("winHeight", 600);
	m_sTitle  = ParamList.GetString("winTitle", "No Title");
}

WindowWin32::~WindowWin32()
{ }

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam);
void WindowWin32::Initialize()
{
	HINSTANCE hInstance = Win32::GetAppInstance();

	WinClassMaker wcm(hInstance, MY_WNDCLS_NAME, WndProc);
	wcm.Register();

	WinMaker wm(hInstance, MY_WNDCLS_NAME);
	wm.SetSize(m_iWidth, m_iHeight);
	m_hWnd = wm.Create(m_sTitle.c_str(), this);

	// init renderer
	if (GetRenderer().get())
		GetRenderer()->Initialize(this);
}

void WindowWin32::Run()
{
	::ShowWindow(m_hWnd, SW_SHOWNORMAL);
	::UpdateWindow(m_hWnd);

	MSG msg;
	ZeroMemory(&msg, sizeof(msg));
	while (msg.message != WM_QUIT)
	{
		if (::PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
		{
			::DispatchMessage(&msg);
		}
		else
		{
			RendererPtr pRenderer = GetRenderer();
			if (pRenderer.get())
				pRenderer->Draw();
		}
	}
}

void WindowWin32::Release()
{
	delete this;
}

HWND WindowWin32::GetHWND()
{
	return m_hWnd;
}

//
// Window Message Handler
//

static LRESULT CALLBACK WndProc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	static WindowWin32* pWin = NULL;
	switch (message)
	{
	case WM_NCCREATE:
		{
		CREATESTRUCT *cs = reinterpret_cast<CREATESTRUCT *>(lParam);
		pWin = reinterpret_cast<WindowWin32 *>(cs->lpCreateParams);
		}
		break;

	case WM_DESTROY:
		if (pWin->GetController().get())
			pWin->GetController()->OnDestroy();
		::PostQuitMessage(0);
		return 0;

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
	}

     return DefWindowProc (hWnd, message, wParam, lParam) ;
}

} // END namespace