#include "Box.hpp"
#include "Desktop.hpp"
#include <assert.h>
using namespace KG;

Desktop *desktop;

void
createWorld()
{
	desktop = new Desktop;
	desktop->setProperty(kDesktopColor, 0, 255, 0);

	Box *box = new Box(10, 10, 200, 100);
	box->setProperty(kBoxColor, 255, 0, 0);
	desktop->addChild(box);
}

void
renderWorld(SDL_Renderer *renderer)
{
	//int x, y;
	//SDL_GetMouseState(&x, &y);
	//printf("mouse: %d, %d\n", x, y);

	desktop->draw(renderer);
	SDL_RenderPresent(renderer);
}

int
main(int argc, char *argv[])
{
	SDL_Window *window;
	SDL_Renderer *renderer;
	int done;
	SDL_Event event;

	if (SDL_Init(SDL_INIT_VIDEO) < 0)
	{
		assert(0 && "SDL_Init failed");
	}

	window = SDL_CreateWindow(NULL, 0, 0, 640, 480, SDL_WINDOW_SHOWN);
	assert(window != 0);

	renderer = SDL_CreateRenderer(window, -1, 0);
	assert(renderer != 0);

	createWorld();

	/* Enter render loop, waiting for user to quit */
	done = 0;
	while (!done)
	{
		while (SDL_PollEvent(&event))
		{
			if (event.type == SDL_QUIT)
			{
				done = 1;
			}
		}
		renderWorld(renderer);
		SDL_Delay(1);
	}

	SDL_Quit();
	return 0;
}

