#include "stdafx.hpp"
#include "MyWindow.hpp"

CAppModule _Module;

int APIENTRY WinMain(HINSTANCE hInst, HINSTANCE hInstPrev, LPSTR lpCmdLine, int nCmdShow)
{
	_Module.Init(NULL, hInst);

	MyWindow wnd;
	if (NULL == wnd.CreateEx())
		return 1;

	wnd.ShowWindow(nCmdShow);
	wnd.UpdateWindow();

	MSG msg;
	while (GetMessage(&msg, NULL, 0, 0) > 0)
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

	_Module.Term();
	return msg.wParam;
}

