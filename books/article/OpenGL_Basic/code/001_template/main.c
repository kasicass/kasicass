#include <stdio.h>
#include <stdlib.h>

#include <SDL/SDL.h>
#include <GL/gl.h>
#include <GL/glu.h>

//
// Init GL
//
void InitGL(void)
{
	glClearColor(0, 0, 0, 0);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(90, 640.0f/480.0f, 1.0, 50.0);
	glViewport(0, 0, 640, 480);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(0.0, 0.0, 1.5, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);	

	glEnable(GL_BLEND);
	printf("vendor: %s\n", glGetString(GL_VENDOR));
}

//
// To render the OpenGL scene
//
void RenderScene(void)
{
	glClear(GL_COLOR_BUFFER_BIT);

	glBegin(GL_TRIANGLES);
		glColor3f(1.0, 0, 0);
		glVertex3f(0.0, 1.0, -2.0);
		glColor3f(0, 1.0, 0);
		glVertex3f(1.0, -1.0, -2.0);
		glColor3f(0, 0, 1.0);
		glVertex3f(-1.0, -1.0, -2.0);
	glEnd();
}

int main(int argc, char *argv[])
{
	if (SDL_Init(SDL_INIT_VIDEO) != 0)
	{
		printf("Unable to init SDL: %s\n", SDL_GetError());
		exit(EXIT_FAILURE);
	}

	atexit(SDL_Quit);

	SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
	SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 5);
	SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 6);
	SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 5);

	if (SDL_SetVideoMode(640, 480, 16, SDL_OPENGL) == NULL)
	{
		printf("Unable to set video mode: %s\n", SDL_GetError());
		exit(EXIT_FAILURE);
	}

	SDL_WM_SetCaption("SDL - OpenGl framework", "OpenGL");

	InitGL();

	// game loop
	bool      loopcontinue = true;
	SDL_Event event;

	while (loopcontinue)
	{
		while (SDL_PollEvent(&event))
		{
			switch (event.type)
			{
			case SDL_KEYDOWN:
				if (event.key.keysym.sym == SDLK_ESCAPE)
					loopcontinue = true;
				break;

			case SDL_QUIT:
				loopcontinue = false;
				break;
			}
		}

		RenderScene();

		SDL_GL_SwapBuffers();
	}
}

