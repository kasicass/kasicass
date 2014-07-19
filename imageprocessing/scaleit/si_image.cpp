#include "si_image.hpp"
#include <assert.h>

namespace si {

Image::Image(int width, int height) : width_(width), height_(height)
{
	data_ = new Color[width * height];
}

Image::~Image()
{
	delete data_;
	data_ = nullptr;
}

Color Image::getColor(int x, int y)
{
	assert(0 <= x && x < width_);
	assert(0 <= y && y < height_);
	return data_[y*width_ + x];
}

void Image::setColor(int x, int y, const Color& c)
{
	assert(0 <= x && x < width_);
	assert(0 <= y && y < height_);
	data_[y*width_ + x] = c;
}

}

