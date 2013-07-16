#include "Box.hpp"
#include <stdio.h>

namespace KG {

Box::Box(int x, int y, int w, int h) : Object2D(x, y, w, h)
{
	color_.r = color_.g = color_.b = 0;
}

Box::~Box()
{
}

void Box::draw(SDL_Renderer *renderer)
{
	SDL_Rect rect;
	rect.x = x_;
	rect.y = y_;
	rect.w = width_;
	rect.h = height_;

	SDL_SetRenderDrawColor(renderer, color_.r, color_.g, color_.b, 255);
	SDL_RenderFillRect(renderer, &rect);
}

void Box::setProperty(int prop, ...)
{
	va_list args;
	if (prop == kBoxColor)
	{
		va_start(args, prop);
		color_.r = (Uint8)va_arg(args, int);
		color_.g = (Uint8)va_arg(args, int);
		color_.b = (Uint8)va_arg(args, int);
		va_end(args);
	}
}

bool Box::getProperty(int prop, void *output)
{
	return true;
}

}

