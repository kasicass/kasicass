#include "si_image.hpp"

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
	return data_[y*width_ + x];
}

void Image::setColor(int x, int y, const Color& c)
{
	data_[y*width_ + x] = c;
}

}

