#pragma once

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <memory>

namespace Ux {

class Window
{
public:
	Window();
	virtual ~Window();

	void show();

	virtual void onDestroy();

private:
	HWND hWnd_;
};
typedef std::shared_ptr<Window> WindowPtr;

WindowPtr createWindow();

}
