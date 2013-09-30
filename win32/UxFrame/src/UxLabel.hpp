#pragma once

#include "UxComponent.hpp"
#include <string>

namespace Ux {

class Label : public Component
{
public:
	Label();
	virtual ~Label();

	// my methods
	const std::wstring& text() const;
	void text(const std::wstring& t);

	float fontSize() const;
	void fontSize(float sz);

	const std::wstring& fontFamily() const;
	void fontFamily(const std::wstring& f);

	Gdiplus::Color fontColor() const;
	void fontColor(const Gdiplus::Color& c);

	// Component methods
	virtual void onDraw(Gdiplus::Graphics& g, int x, int y);

private:
	std::wstring text_;
	std::wstring fontFamily_;
	float fontSize_;
	Gdiplus::Color fontColor_;
};
typedef std::shared_ptr<Label> LabelPtr;

LabelPtr createLabel();

}
