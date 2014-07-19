#include "si_filter.hpp"
#include <assert.h>

namespace si {

Filter::Filter()
{
	rate_ = -1.0;
}

Filter::~Filter()
{
}

void Filter::setInfo(ImagePtr src, float rate)
{
	source_ = src;
	rate_   = rate;
}

Color Filter::pick(int x, int y)
{
	assert(source_);
	assert(rate_ > 0.f);

	return realPick(x, y);	
}

class LinearFilter : public Filter
{
protected:
	virtual Color realPick(int x, int y)
	{
		int realX = int(x / rate_);
		int realY = int(y / rate_);
		return source_->getColor(realX, realY);
	}
};

FilterPtr makeFilter(const std::string& ftype)
{
	if (ftype == "linear") return std::make_shared<LinearFilter>();
	else return std::make_shared<LinearFilter>();
}

}

