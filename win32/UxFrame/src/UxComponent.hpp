#pragma once

#include <Windows.h>
#include <GdiPlus.h>
#include <memory>

namespace Ux {

class Component
{
public:
	virtual bool visible() = 0;
	virtual void onDraw(Gdiplus::Graphics& g) = 0;

	HWND getHWND() const;

private:
	HWND hWnd_;
};
typedef std::shared_ptr<Component> ComponentPtr;

}
