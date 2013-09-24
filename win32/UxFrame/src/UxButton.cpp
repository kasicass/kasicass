#include "UxButton.hpp"
#include "UxUtil.hpp"
#include "UxApp.hpp"
#include <unordered_map>

namespace Ux {

static LRESULT CALLBACK ButtonWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
Button::Button(UINT id)
{
	bitmap_.load(id);
}

Button::~Button()
{
}

void Button::createMe(HWND hParent)
{
	HINSTANCE hInst = App::instance().getHINSTANCE();

	WinClassMaker wcm(hInst, "UxButton", ButtonWndProc);
	wcm.registerMe();

	ChildWinMaker wm(hInst, "UxButton");
	wm.setPos(x_, y_);
	wm.setSize(bitmap_.width(), bitmap_.height());
	hWnd_ = wm.create(hParent, this);

	::ShowWindow(hWnd_, SW_SHOWNORMAL);
	::UpdateWindow(hWnd_);
}

void Button::onDraw(Gdiplus::Graphics& g)
{
	g.DrawImage(bitmap_, x_, y_, bitmap_.width(), bitmap_.height());
}

void Button::onDestroy()
{
	bitmap_.release();
}

ButtonPtr createButton(UINT id)
{
	return std::make_shared<Button>(id);
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

	case WM_LBUTTONUP:
		OutputDebugString("kasicass xx WM_LBUTTONUP\n");
		return 0;

	case WM_MOUSEHOVER:
		OutputDebugString("kasicass xx WM_MOUSEHOVER\n");
		return 0;

	case WM_DRAWITEM:
		OutputDebugString("kasicass xx WM_DRAWITEM\n");
		return 0;
	}

	return DefWindowProc(hWnd, message, wParam, lParam) ;
}

}
