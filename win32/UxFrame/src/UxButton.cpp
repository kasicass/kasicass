#include "UxButton.hpp"
#include "UxUtil.hpp"
#include "UxGlobal.hpp"
#include <unordered_map>


namespace Ux {

static LRESULT CALLBACK ButtonWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
Button::Button(UINT normal, UINT down, UINT over, UINT disable) :
	mouseTracking_(false),
	mouseDown_(false),
	mouseOver_(false),
	isDisable_(false),
	currState_(ST_NORMAL)
{
	bitmaps_[ST_NORMAL].load(normal);
	bitmaps_[ST_DOWN].load(down);
	bitmaps_[ST_OVER].load(over);
	bitmaps_[ST_DISABLE].load(disable);
}

Button::~Button()
{
}

void Button::createMe(HWND hParent)
{
	Component::createMe(hParent);

	HINSTANCE hInst = Global::getHINSTANCE();

	WinClassMaker wcm(hInst, "UxButton", ButtonWndProc);
	wcm.registerMe();

	ChildWinMaker wm(hInst, "UxButton");
	wm.setPos(x_, y_);
	wm.setSize(bitmaps_[ST_NORMAL].width(), bitmaps_[ST_NORMAL].height());
	hWnd_ = wm.create(hParent, this);

	::ShowWindow(hWnd_, SW_SHOWNORMAL);
	::UpdateWindow(hWnd_);
}

void Button::onDraw(Gdiplus::Graphics& g, int x, int y)
{
	g.DrawImage(bitmaps_[currState_], x+x_, y+y_);
}

bool Button::disable() const
{
	return isDisable_;
}

void Button::disable(bool d)
{
	if (isDisable_ == d)
		return;

	isDisable_ = d;
	updateButtonState();
}

void Button::setClickFunc(std::function<void(Button*)> fn)
{
	clickFunc_ = fn;
}

void Button::onLButtonDown()
{
	mouseDown_ = true;
	updateButtonState();
}

void Button::onLButtonUp()
{
	mouseDown_ = false;
	updateButtonState();

	if (clickFunc_)
	{
		clickFunc_(this);
	}
}

void Button::onMouseMove(int x, int y)
{
	if (!mouseTracking_)
	{
		TRACKMOUSEEVENT tme;
		tme.cbSize = sizeof(tme);
		tme.hwndTrack = hWnd_;
		tme.dwFlags = TME_LEAVE | TME_HOVER;
		tme.dwHoverTime = HOVER_DEFAULT;
		mouseTracking_ = (::TrackMouseEvent(&tme) == TRUE);
	}
}

void Button::onMouseHover()
{
	mouseOver_ = true;
	updateButtonState();
}

void Button::onMouseLeave()
{
	mouseOver_ = false;
	mouseTracking_ = false;
	updateButtonState();
}

void Button::updateButtonState()
{
	if (isDisable_) currState_ = ST_DISABLE;
	else if (mouseDown_) currState_ = ST_DOWN;
	else if (mouseOver_) currState_ = ST_OVER;
	else currState_ = ST_NORMAL;
	notifyRedraw();
}

static LRESULT CALLBACK ButtonWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	static std::tr1::unordered_map<HWND, Button*> btnmap;

	switch (message)
	{
	case WM_NCCREATE:
		{
		CREATESTRUCT *cs = reinterpret_cast<CREATESTRUCT *>(lParam);
		btnmap[hWnd] = reinterpret_cast<Button *>(cs->lpCreateParams);
		}
		break;

	case WM_LBUTTONDOWN:
		btnmap[hWnd]->onLButtonDown();
		return 0;

	case WM_LBUTTONUP:
		btnmap[hWnd]->onLButtonUp();
		return 0;

	case WM_MOUSEMOVE:
		{
		POINTS p = MAKEPOINTS(lParam);
		btnmap[hWnd]->onMouseMove(p.x, p.y);
		}
		return 0;

	case WM_MOUSEHOVER:
		{
		btnmap[hWnd]->onMouseHover();
		}
		return 0;

	case WM_MOUSELEAVE:
		{
		btnmap[hWnd]->onMouseLeave();
		}
		return 0;

	default:
		break;
	}

	return DefWindowProc(hWnd, message, wParam, lParam) ;
}


//
// createButton()
//

ButtonPtr createButton(UINT normal)
{
	return std::make_shared<Button>(normal, normal, normal, normal);
}

ButtonPtr createButton(UINT normal, UINT down)
{
	return std::make_shared<Button>(normal, down, normal, normal);
}

ButtonPtr createButton(UINT normal, UINT down, UINT over)
{
	return std::make_shared<Button>(normal, down, over, normal);
}

ButtonPtr createButton(UINT normal, UINT down, UINT over, UINT disable)
{
	return std::make_shared<Button>(normal, down, over, disable);
}

}
