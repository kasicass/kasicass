#pragma once

#include "UxComponent.hpp"
#include "UxGdiPlusBitmap.hpp"

namespace Ux {

class ProgressBar : public Component
{
public:
	ProgressBar(UINT fg, UINT bg);
	virtual ~ProgressBar();

	// my methods
	float percent() const;
	void percent(float p);

	// Component methods
	virtual void onDraw(Gdiplus::Graphics& g, int x, int y);

private:
	GdiPlusBitmapResource foreground_;
	GdiPlusBitmapResource background_;
	float percent_; // 0.0 ~ 1.0
};
typedef std::shared_ptr<ProgressBar> ProgressBarPtr;

ProgressBarPtr createProgressBar(UINT fg, UINT bg);

}
