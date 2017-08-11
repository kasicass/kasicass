#include <windows.h>

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
	PostMessage(HWND_BROADCAST, WM_KEYDOWN, 'S', 0);
	PostMessage(HWND_BROADCAST, WM_KEYUP, 'S', 0);
	return 0;
}