#pragma once

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

namespace Ux {

//
// WinClassMaker & WinMaker
//

class WinClassMaker
{
public:
	WinClassMaker(HINSTANCE hInstance, LPCTSTR sClassName, WNDPROC WndProc);
	void registerMe();

private:
	WinClassMaker(const WinClassMaker&);
	WinClassMaker& operator=(const WinClassMaker&);

private:
	WNDCLASSEX clsEx_;
};

class WinMaker
{
public:
	WinMaker(HINSTANCE hInstance, LPCTSTR sClassName);

	void setPos(int x, int y);
	void setSize(int iWidth, int iHeight);

	HWND create(LPCTSTR sTitle, void* pData);

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
