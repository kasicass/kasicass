#include "KApp.h"
#include "KMainWindow.h"

KMyApp myApp;

BOOL KMyApp::InitInstance()
{
	Gdiplus::GdiplusStartupInput input;
	Gdiplus::GdiplusStartup(&token_, &input, NULL);

	m_pMainWnd = new KMainWindow;

	m_pMainWnd->ShowWindow(m_nCmdShow);
	m_pMainWnd->UpdateWindow();
	return TRUE;
}

int KMyApp::ExitInstance()
{
	Gdiplus::GdiplusShutdown(token_);
	return CWinApp::ExitInstance();
}

