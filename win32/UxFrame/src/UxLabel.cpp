#include "UxLabel.hpp"

namespace Ux {

Label::Label() :
	fontFamily_(L"Times New Roman"),
	fontSize_(18.0f),
	fontColor_(255, 255, 255)
{
}

Label::~Label()
{
}

const std::wstring& Label::text() const
{
	return text_;
}

void Label::text(const std::wstring& t)
{
	text_ = t;
	notifyRedraw();
}

float Label::fontSize() const
{
	return fontSize_;
}

void Label::fontSize(float sz)
{
	fontSize_ = sz;
	notifyRedraw();
}

const std::wstring& Label::fontFamily() const
{
	return fontFamily_;
}

void Label::fontFamily(const std::wstring& f)
{
	fontFamily_ = f;
	notifyRedraw();
}

Gdiplus::Color Label::fontColor() const
{
	return fontColor_;
}

void Label::fontColor(const Gdiplus::Color& c)
{
	fontColor_ = c;
	notifyRedraw();
}

void Label::onDraw(Gdiplus::Graphics& g, int x, int y)
{
	Gdiplus::FontFamily  fontFamily(fontFamily_.c_str());
	Gdiplus::Font        font(&fontFamily, fontSize_, Gdiplus::FontStyleRegular, Gdiplus::UnitPixel);
	Gdiplus::PointF      pointF(float(x+x_), float(y+y_));
	Gdiplus::SolidBrush  solidBrush(fontColor_);

	g.DrawString(text_.c_str(), text_.length(), &font, pointF, &solidBrush);
}

LabelPtr createLabel()
{
	return std::make_shared<Label>();
}

}