#pragma once

#include "UxComponent.hpp"
#include "UxGdiPlusBitmap.hpp"

namespace Ux {

class Button : public Component
{
public:
	typedef void (*CLICK_FUNC)(Button*);

public:
	Button(UINT normal, UINT down, UINT over, UINT disable);
	virtual ~Button();

	// my methods
	bool disable() const;
	void disable(bool d);

	void setClickFunc(CLICK_FUNC fn);

	// WndProc event
	void onLButtonDown();
	void onLButtonUp();
	void onMouseMove(int x, int y);
	void onMouseHover();
	void onMouseLeave();

	void updateButtonState();

	// Component methods
	virtual void createMe(HWND hParent);
	virtual void onDraw(Gdiplus::Graphics& g, int x, int y);

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
	CLICK_FUNC clickFunc_;
};
typedef std::shared_ptr<Button> ButtonPtr;

ButtonPtr createButton(UINT normal);
ButtonPtr createButton(UINT normal, UINT down);
ButtonPtr createButton(UINT normal, UINT down, UINT over);
ButtonPtr createButton(UINT normal, UINT down, UINT over, UINT disable);

}
