#ifndef K1R_WINDOW_WIN32_H
#define K1R_WINDOW_WIN32_H

#include "K1RWindow.h"
#include "K1RWin32Platform.h"
#include <string>

namespace K1R {

class PropList;

class WindowWin32 : public Window
{
public:
	WindowWin32(PropList& ParamList);
	virtual ~WindowWin32();

	virtual void Initialize();
	virtual void Run();
	virtual void Release();

	HWND GetHWND();

private:
	int m_iWidth, m_iHeight;
	std::string m_sTitle;

	HWND m_hWnd;
};

}

#endif