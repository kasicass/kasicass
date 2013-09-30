#include "UxProgressBar.hpp"

namespace Ux {

ProgressBar::ProgressBar(UINT fg, UINT bg) : percent_(0.0f)
{
	foreground_.load(fg);
	background_.load(bg);
}

ProgressBar::~ProgressBar()
{
}

float ProgressBar::percent() const
{
	return percent_;
}

void ProgressBar::percent(float p)
{
	if (p < 0.0f) percent_ = 0.0f;
	else if (p > 1.0f) percent_ = 1.0f;
	else percent_ = p;

	notifyRedraw();
}

void ProgressBar::onDraw(Gdiplus::Graphics& g, int x, int y)
{
	g.DrawImage(background_, x+x_, y+y_);

	int w = int(foreground_.width() * percent_);
	int h = foreground_.height();
	g.DrawImage(foreground_, x+x_, y+y_, 0, 0, w, h, Gdiplus::UnitPixel);
}

ProgressBarPtr createProgressBar(UINT fg, UINT bg)
{
	return std::make_shared<ProgressBar>(fg, bg);
}

}