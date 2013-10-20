#include "stdafx.hpp"
#include "MyDialog.hpp"

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hInstPrev, LPSTR szCmdLine, int nCmdShow)
{
	MyDialog dlg;
	dlg.DoModal();
	return 0;
}

