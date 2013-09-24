#pragma once

#include "UxComponent.hpp"
#include "UxGdiPlusBitmap.hpp"

namespace Ux {

class Button : public Component
{
public:
	Button(UINT id);
	virtual ~Button();

	virtual void createMe(HWND hParent);
	virtual void onDraw(Gdiplus::Graphics& g);
	virtual void onDestroy();

private:
	GdiPlusBitmapResource bitmap_;
};
typedef std::shared_ptr<Button> ButtonPtr;

ButtonPtr createButton(UINT id);

}
