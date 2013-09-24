#include "UxButton.hpp"

namespace Ux {

Button::Button(UINT id)
{
	bitmap_.load(id);
}

Button::~Button()
{
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

}
