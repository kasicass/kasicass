#ifndef TESTGUI_BOX_HPP
#define TESTGUI_BOX_HPP

#include "Object2D.hpp"

namespace KG {

enum
{
	kBoxColor = 'boxc',
	kBoxAlpha = 'boxa',
};

class Box : public Object2D
{
public:
	Box(int x, int y, int w, int h);
	virtual ~Box();

	virtual void draw(SDL_Renderer *renderer);
	virtual void setProperty(int prop, ...);
	virtual bool getProperty(int prop, void *output);

private:
	SDL_Color color_;
};

}

#endif
