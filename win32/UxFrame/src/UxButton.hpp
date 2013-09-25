#pragma once

#include "UxComponent.hpp"
#include "UxGdiPlusBitmap.hpp"

namespace Ux {

class Button : public Component
{
public:
	Button(UINT normal, UINT down, UINT over, UINT disable);
	virtual ~Button();

	virtual void createMe(HWND hParent);
	virtual void onDraw(Gdiplus::Graphics& g);
	virtual void onDestroy();

	bool disable() const;
	void disable(bool d);

	// WndProc event
	void onLButtonDown();
	void onLButtonUp();
	void onMouseMove(int x, int y);
	void onMouseHover();
	void onMouseLeave();

	void updateButtonState();

private:
	enum BUTTON_STATE
	{
		ST_NORMAL = 0,
		ST_DOWN,
		ST_OVER,
		ST_DISABLE,
		ST_NUM
	};

private:
	GdiPlusBitmapResource bitmaps_[ST_NUM];
	BUTTON_STATE currState_;
	bool mouseTracking_;
	bool mouseDown_;
	bool mouseOver_;
	bool isDisable_;
};
typedef std::shared_ptr<Button> ButtonPtr;

ButtonPtr createButton(UINT normal);
ButtonPtr createButton(UINT normal, UINT down);
ButtonPtr createButton(UINT normal, UINT down, UINT over);
ButtonPtr createButton(UINT normal, UINT down, UINT over, UINT disable);

}
