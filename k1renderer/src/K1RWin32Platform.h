#ifndef K1R_WINDOW_WIN32PLATFORM_H
#define K1R_WINDOW_WIN32PLATFORM_H

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

namespace K1R {

namespace Win32
{
	HINSTANCE GetAppInstance();
	void SetAppInstance(HINSTANCE hInst);
}

//
// WinClassMaker & WinMaker
//

class WinClassMaker
{
public:
	WinClassMaker (HINSTANCE hInstance, LPCTSTR sClassName, WNDPROC WndProc);
	void Register();

private:
	WinClassMaker(const WinClassMaker&);
	WinClassMaker& operator=(const WinClassMaker&);

private:
	WNDCLASSEX m_ClassEx;
};

class WinMaker
{
public:
	WinMaker(HINSTANCE hInstance, LPCTSTR sClassName);

	void SetPos(int x, int y);
	void SetSize(int iWidth, int iHeight);

	HWND Create(LPCTSTR sTitle, void* pData);

private:
	WinMaker(const WinMaker&);
	WinMaker& operator=(const WinMaker&);

private:
	HINSTANCE	m_hInstance;
	LPCTSTR		m_sClassName;
	DWORD		m_dwStyle;
	DWORD       m_dwExStyle;
	int			m_x;
	int			m_y;
	int			m_iWidth;
	int			m_iHeight;
};

}

#endif