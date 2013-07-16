#ifndef TESTGUI_DESKTOP_HPP
#define TESTGUI_DESKTOP_HPP

#include "Object2D.hpp"
#include <vector>

namespace KG {

enum
{
	kDesktopColor = 'dskc',
};

class Desktop : public Object2D
{
public:
	Desktop();
	virtual ~Desktop();

	virtual void draw(SDL_Renderer *renderer);
	virtual void setProperty(int prop, ...);
	virtual bool getProperty(int prop, void *output);

	virtual bool isContainer() { return true; }
	virtual void addChild(Object2D *o);
	virtual void removeChild(Object2D *o);

private:
	std::vector<Object2D*> children_;
	SDL_Color bgcolor_;
};

}

#endif
