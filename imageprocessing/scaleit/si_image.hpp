#pragma once

#include <memory>

namespace si {

struct Color {
	int r, g, b;

	Color() : r(0), g(0), b(0) {}
	Color(int r0, int g0, int b0) : r(r0), g(g0), b(b0) {}
};

class Image {
public:
	Image(int width, int height);
	~Image();

	int width() const { return width_; }
	int height() const { return height_; }

	Color getColor(int x, int y);
	void setColor(int x, int y, const Color& c);

private:	
	int width_, height_;
	Color *data_;
};
typedef std::shared_ptr<Image> ImagePtr;

}
