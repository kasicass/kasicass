#include "UxUtil.hpp"
#include <assert.h>

namespace Ux {

//
// WinClassMaker
//

WinClassMaker::WinClassMaker(HINSTANCE hInstance, LPCTSTR sClassName, WNDPROC WndProc)
{
	clsEx_.cbSize        = sizeof(WNDCLASSEX); 
	clsEx_.hInstance     = hInstance;
	clsEx_.lpszClassName = sClassName;
	clsEx_.lpfnWndProc   = WndProc;
	clsEx_.style         = CS_HREDRAW | CS_VREDRAW;
	clsEx_.cbClsExtra    = 0;
	clsEx_.cbWndExtra    = 0;
	clsEx_.hCursor       = LoadCursor(NULL, IDC_ARROW);
	clsEx_.hbrBackground = NULL;
	clsEx_.lpszMenuName  = (LPCTSTR)NULL;
	clsEx_.hIcon         = NULL;
	clsEx_.hIconSm       = NULL;
}

void WinClassMaker::registerMe()
{
	ATOM ret = ::RegisterClassEx(&clsEx_);
	assert(ret != 0);
}


//
// WinMaker
//

WinMaker::WinMaker(HINSTANCE hInstance, LPCTSTR sClassName)
  : m_hInstance(hInstance),
    m_sClassName(sClassName),
    m_dwStyle(WS_OVERLAPPED | WS_SYSMENU),
	m_dwExStyle(WS_EX_LAYERED),
	m_x(100), m_y(100),
	m_iWidth(800), m_iHeight(600)
 { }

void WinMaker::setPos(int x, int y)
{
	m_x = x;
	m_y = y;
}

void WinMaker::setSize(int iWidth, int iHeight)
{
	m_iWidth  = iWidth;
	m_iHeight = iHeight;
}

HWND WinMaker::create(LPCTSTR sTitle, void *pData)
{
	HWND hWnd = ::CreateWindowEx(
		m_dwExStyle,
		m_sClassName,
		sTitle,
		m_dwStyle,
		m_x,
		m_y,
		m_iWidth,
		m_iHeight,
		NULL,			// parent
		NULL,			// menu
		m_hInstance,
		pData);			// data

	assert(hWnd != 0);
	return hWnd;
}

}
