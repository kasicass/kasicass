#include "WinUtil.h"


//
// WinClassMaker
//

WinClassMaker::WinClassMaker(HINSTANCE hInstance, LPCTSTR sClassName, WNDPROC WndProc)
{
	m_ClassEx.cbSize        = sizeof(WNDCLASSEX); 
	m_ClassEx.hInstance     = hInstance;
	m_ClassEx.lpszClassName = sClassName;
	m_ClassEx.lpfnWndProc   = WndProc;
	m_ClassEx.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS;
	m_ClassEx.cbClsExtra    = 0;
	m_ClassEx.cbWndExtra    = 0;
	m_ClassEx.hCursor       = LoadCursor(NULL, IDC_ARROW);
	m_ClassEx.hbrBackground = NULL;
	m_ClassEx.lpszMenuName  = (LPCTSTR)NULL;
	m_ClassEx.hIcon         = NULL;
	m_ClassEx.hIconSm       = NULL;
}

void WinClassMaker::Register()
{
	if ( ::RegisterClassEx(&m_ClassEx) == 0 )
		throw "RegisterClass failed";
}


//
// WinMaker
//
#define MY_WINDOW_STYLE		(WS_OVERLAPPED|WS_SYSMENU|WS_CAPTION|WS_MINIMIZEBOX)

WinMaker::WinMaker(HINSTANCE hInstance, LPCTSTR sClassName)
  : m_hInstance(hInstance),
    m_sClassName(sClassName),
    m_dwStyle(MY_WINDOW_STYLE),
	m_dwExStyle(0),
	m_x(100), m_y(100),
	m_iWidth(800), m_iHeight(600)
 { }

void WinMaker::SetPos(int x, int y)
{
	m_x = x;
	m_y = y;
}

void WinMaker::SetSize(int iWidth, int iHeight)
{
	m_iWidth  = iWidth;
	m_iHeight = iHeight;
}

HWND WinMaker::Create(LPCTSTR sTitle, void *pData)
{
	int width  = m_iWidth;
	int height = m_iHeight;

	RECT rect = {0, 0, width, height};
	if (::AdjustWindowRect(&rect, m_dwStyle, FALSE))  // make the client area right~
	{
		width  = rect.right - rect.left;
		height = rect.bottom - rect.top;
	}

	HWND hWnd = ::CreateWindowEx(
		m_dwExStyle,
		m_sClassName,
		sTitle,
		m_dwStyle,
		m_x,
		m_y,
		width,
		height,
		NULL,			// parent
		NULL,			// menu
		m_hInstance,
		pData);			// data

	if (hWnd == 0)
		throw "Window Creation Failed";

	return hWnd;
}
