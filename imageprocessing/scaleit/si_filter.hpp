#pragma once

#include "si_image.hpp"
#include <string>

namespace si {

class Filter
{
public:
	Filter();
	virtual ~Filter();

	void setInfo(ImagePtr src, float rate);
	Color pick(int x, int y);

protected:
	virtual Color realPick(int x, int y) = 0;

	ImagePtr source_;
	float rate_;
};
typedef std::shared_ptr<Filter> FilterPtr;

FilterPtr makeFilter(const std::string& ftype);

}

