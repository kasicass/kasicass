#include "stdafx.hpp"
#include "MyWindow.hpp"

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hInstPrev, LPSTR szCmdLine, int nCmdShow)
{
	MyWindow wnd;
	if (NULL == wnd.Create(NULL, NULL, _T("ATL Window")))
		return 1;

	wnd.ShowWindow(nCmdShow);
	wnd.UpdateWindow();

	MSG msg;
	while (GetMessage(&msg, NULL, 0, 0) > 0)
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

	return msg.wParam;
}

