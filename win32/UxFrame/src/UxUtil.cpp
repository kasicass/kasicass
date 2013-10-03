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
	clsEx_.hCursor       = ::LoadCursor(NULL, IDC_ARROW);
	clsEx_.hbrBackground = NULL;
	clsEx_.lpszMenuName  = (LPCTSTR)NULL;
	clsEx_.hIcon     = NULL;
	clsEx_.hIconSm   = NULL;
}

void WinClassMaker::registerMe()
{
	::RegisterClassEx(&clsEx_);

	// ATOM ret = ::RegisterClassEx(&clsEx_);
	// assert(ret != 0);
}


//
// WinMaker
//

WinMaker::WinMaker(HINSTANCE hInstance, LPCTSTR sClassName)
  : m_hInstance(hInstance),
    m_sClassName(sClassName),
    m_dwStyle(WS_OVERLAPPED|WS_SYSMENU),
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
	//RECT rect = {0, 0, m_iWidth, m_iHeight};
	//AdjustWindowRect(&rect, m_dwStyle, TRUE);

	HWND hWnd = ::CreateWindowEx(
		m_dwExStyle,
		m_sClassName,
		sTitle,
		m_dwStyle,
		m_x,
		m_y,
		m_iWidth, //rect.right - rect.left,
		m_iHeight, //rect.bottom - rect.top + 300,
		NULL,			// parent
		NULL,			// menu
		m_hInstance,
		pData);			// data

	// SetWindowLong(hWnd, GWL_STYLE, 0); // remove all style (no title bar)

	assert(hWnd != 0);
	return hWnd;
}


//
// ChildWinMaker
//

ChildWinMaker::ChildWinMaker(HINSTANCE hInstance, LPCTSTR sClassName)
  : m_hInstance(hInstance),
    m_sClassName(sClassName),
    m_dwStyle(WS_CHILDWINDOW),
	m_dwExStyle(0),
	m_x(0), m_y(0),
	m_iWidth(50), m_iHeight(50)
 { }

void ChildWinMaker::setPos(int x, int y)
{
	m_x = x;
	m_y = y;
}

void ChildWinMaker::setSize(int iWidth, int iHeight)
{
	m_iWidth  = iWidth;
	m_iHeight = iHeight;
}

HWND ChildWinMaker::create(HWND hParent, void* pData)
{
	HWND hWnd = ::CreateWindowEx(
		m_dwExStyle,
		m_sClassName,
		NULL,
		m_dwStyle,
		m_x,
		m_y,
		m_iWidth,
		m_iHeight,
		hParent,		// parent
		NULL,			// menu
		m_hInstance,
		pData);			// data

	assert(hWnd != 0);
	return hWnd;
}

}
