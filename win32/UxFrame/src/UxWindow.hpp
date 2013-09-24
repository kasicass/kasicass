// UxWindow -- 1:* -- UxComponent
//                      UxButton
//                      UxLabel
//                      UxProgressBar
//                      UxIE

#pragma once

#include <Windows.h>
#include <memory>
#include <vector>
#include "UxComponent.hpp"
#include "UxGdiPlusBitmap.hpp"

namespace Ux {

class Window
{
public:
	Window(UINT id);
	virtual ~Window();

	void draw();
	void show();

	virtual void onDestroy();

private:
	HWND hWnd_;
	GdiPlusBitmapResource bgImage_;
	std::vector<ComponentPtr> children_;
};
typedef std::shared_ptr<Window> WindowPtr;

}
