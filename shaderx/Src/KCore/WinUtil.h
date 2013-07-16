#ifndef KCORE_WINUTIL_H
#define KCORE_WINUTIL_H

#include <windows.h>

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

	HWND Create(LPCTSTR sTitle, void* pData = NULL);

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

#endif