#include "Desktop.hpp"
#include <algorithm>

namespace KG {

Desktop::Desktop() : Object2D(0, 0, 640, 480)
{
	bgcolor_.r = bgcolor_.g = bgcolor_.b = 0;
}

Desktop::~Desktop()
{
	std::vector<Object2D*>::iterator it  = children_.begin();
	std::vector<Object2D*>::iterator end = children_.end();
	for (; it != end; ++it)
	{
		delete *it;
	}
	children_.clear();
}

void Desktop::draw(SDL_Renderer *renderer)
{
	// draw self
	SDL_SetRenderDrawColor(renderer, bgcolor_.r, bgcolor_.g, bgcolor_.b, 255);
	SDL_RenderFillRect(renderer, NULL);

	// draw children
	std::vector<Object2D*>::iterator it  = children_.begin();
	std::vector<Object2D*>::iterator end = children_.end();
	for (; it != end; ++it)
	{
		(*it)->draw(renderer);
	}
}

void Desktop::setProperty(int prop, ...)
{
	va_list args;
	if (prop == kDesktopColor)
	{
		va_start(args, prop);
		bgcolor_.r = (Uint8)va_arg(args, int);
		bgcolor_.g = (Uint8)va_arg(args, int);
		bgcolor_.b = (Uint8)va_arg(args, int);
		va_end(args);
	}
}

bool Desktop::getProperty(int prop, void *output)
{
	return true;
}

void Desktop::addChild(Object2D *c)
{
	children_.push_back(c);
}

void Desktop::removeChild(Object2D *c)
{
	std::vector<Object2D*>::iterator it = std::find(children_.begin(), children_.end(), c);
	if (it != children_.end())
		children_.erase(it);
}

}
