#include "si_filter.hpp"
#include <cassert>
#include <cmath>
#include <iostream>

namespace si {

//
// Filter
//

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


//
// Utility
//

static bool almostEqual(float a, float b)
{
	return std::abs(a - b) > 0.0001f;
}


//
// LinearFilter
//

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


//
// BilinearFilter
//

class BilinearFilter : public Filter
{
protected:
	virtual Color realPick(int x, int y)
	{
		assert(rate_ >= 1.f);

		float x0 = float(x) / rate_;
		float y0 = float(y) / rate_;
		const float almostOne = 0.9999f;

		Color c00 = source_->getColor(int(x0), int(y0));
		Color c10 = ((int(x0)+1 < source_->width()) ? source_->getColor(int(x0+almostOne), int(y0)) : c00);
		Color c01 = ((int(y0)+1 < source_->height()) ? source_->getColor(int(x0), int(y0+almostOne)) : c00);
		Color c11 = (((int(x0)+1 < source_->width()) && (int(y0)+1 < source_->height())) ? source_->getColor(int(x0+almostOne), int(y0+almostOne)) : c00);

		float fractionalx = x0 - float(int(x0));
		float fractionaly = y0 - float(int(y0));

		Color outX, outY;
		outX.r = c00.r * fractionalx + c10.r * (1.f - fractionalx);
		outX.g = c00.g * fractionalx + c10.g * (1.f - fractionalx);
		outX.b = c00.b * fractionalx + c10.b * (1.f - fractionalx);
		outY.r = c01.r * fractionalx + c11.r * (1.f - fractionalx);
		outY.g = c01.g * fractionalx + c11.g * (1.f - fractionalx);
		outY.b = c01.b * fractionalx + c11.b * (1.f - fractionalx);

		Color out;
		out.r = outX.r * fractionaly + outY.r * (1.f - fractionaly);
		out.g = outX.g * fractionaly + outY.g * (1.f - fractionaly);
		out.b = outX.b * fractionaly + outY.b * (1.f - fractionaly);

		return out;
	}
};


//
// factory func
//

#define BEGIN_MATCH(name, filter) if (ftype == name) { std::cout << "filter: " << name << std::endl; \
	return std::make_shared<filter>(); }

#define DO_MATCH(name, filter) else if (ftype == name) { std::cout << "filter: " << name << std::endl; \
	return std::make_shared<filter>(); }

#define END_MATCH else { std::cout << "filter: default(linear)" << std::endl; return std::make_shared<LinearFilter>(); }

FilterPtr makeFilter(const std::string& ftype)
{
	BEGIN_MATCH("linear", LinearFilter)
	DO_MATCH("bilinear", BilinearFilter)
	END_MATCH
}

}

