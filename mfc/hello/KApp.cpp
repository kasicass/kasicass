#include "KApp.h"
#include "KMainWindow.h"

KMyApp myApp;

BOOL KMyApp::InitInstance()
{
	m_pMainWnd = new KMainWindow;

	m_pMainWnd->ShowWindow(m_nCmdShow);
	m_pMainWnd->UpdateWindow();
	return TRUE;
}

