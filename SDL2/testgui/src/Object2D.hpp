#ifndef TESTGUI_OBJECT2D_HPP
#define TESTGUI_OBJECT2D_HPP

#include <SDL2/SDL.h>

namespace KG {

enum
{
	kMessageMouseMove = 1,
};

class Object2D
{
public:
	Object2D(int x, int y, int w, int h) : x_(x), y_(y), width_(w), height_(h), hover_(false) {}
	virtual ~Object2D() {}

	int x() { return x_; }
	void x(int x) { x_ = x; }
	int y() { return y_; }
	void y(int y) { y_ = y; }

	int width() { return width_; }
	void width(int w) { width_ = w; }
	int height() { return height_; }
	void height(int h) { height_ = h; }

	bool hover() { return hover_; }
	void hover(bool h) { hover_ = h; }

	void sendMouseMessage(int x, int y);
	virtual void handleMessage(int message, void *data) = 0;

	virtual void draw(SDL_Renderer *renderer) = 0;
	virtual void setProperty(int prop, ...) = 0;
	virtual bool getProperty(int prop, void *output) = 0;

	virtual bool isContainer() { return false; }
	virtual void addChild(Object2D *o) {}
	virtual void removeChild(Object2D *o) {}

protected:
	int x_, y_;
	int width_, height_;
	bool hover_;
};

}

#endif
