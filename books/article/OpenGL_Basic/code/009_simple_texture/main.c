// simple texture

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <SDL/SDL.h>
#include <GL/gl.h>
#include <GL/glu.h>


//------------------------------------------------------------------------------
// my demo

#define	SCREEN_WIDTH	640
#define	SCREEN_HEIGHT	480

#define	TEX_WIDTH		64
#define	TEX_HEIGHT		64

static GLubyte mytex[TEX_HEIGHT][TEX_WIDTH][4];
static GLuint  texName;

//
// Init GL
//
void InitTexture(void)
{
	int i, j, c;
	for ( i = 0; i < TEX_HEIGHT; i++ )
	{
		for ( j = 0; j < TEX_WIDTH; j++ )
		{
			c = ((((i&0x8)==0)^(j&0x8)==0))*255;
			mytex[i][j][0] = (GLubyte) c;
			mytex[i][j][1] = (GLubyte) c;
			mytex[i][j][2] = (GLubyte) c;
			mytex[i][j][3] = (GLubyte) 255;
		}
	}
}


void InitGL(void)
{
	// set camera
	glViewport(0, 0, (GLsizei) SCREEN_WIDTH, (GLsizei) SCREEN_HEIGHT);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(60.0, (GLfloat) SCREEN_WIDTH / (GLfloat) SCREEN_HEIGHT, 1.0, 30.0);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glTranslatef(0.0, 0.0, -3.6);

	glClearColor(0.5, 0.5, 0.5, 0.0);
	glShadeModel(GL_FLAT);
	glEnable(GL_DEPTH_TEST);

	InitTexture();
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

	glGenTextures(1, &texName);
	glBindTexture(GL_TEXTURE_2D, texName);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, TEX_WIDTH, TEX_HEIGHT, 0, GL_RGBA, GL_UNSIGNED_BYTE, mytex);
}

void RenderScene(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glEnable(GL_TEXTURE_2D);
	glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
	glBindTexture(GL_TEXTURE_2D, texName);

	glBegin(GL_QUADS);
		glTexCoord2f(-1.0, -1.0);		glVertex3f(-2.0, -1.0, 0.0);
		glTexCoord2f(-1.0, 2.0);		glVertex3f(-2.0, 1.0, 0.0);
		glTexCoord2f(2.0, 2.0);		glVertex3f(0.0, 1.0, 0.0);
		glTexCoord2f(2.0, -1.0);		glVertex3f(0.0, -1.0, 0.0);
		glTexCoord2f(0.0, 0.0);		glVertex3f(1.0, -1.0, 0.0);
		glTexCoord2f(0.0, 1.0);		glVertex3f(1.0, 1.0, 0.0);
		glTexCoord2f(1.0, 1.0);		glVertex3f(2.41421, 1.0, -1.41421);
		glTexCoord2f(1.0, 0.0);		glVertex3f(2.41421, -1.0, -1.41421);
	glEnd();
	glFlush();
	glDisable(GL_TEXTURE_2D);
}

void kbd_event(int sym)
{
	switch (sym)
	{
	default:
		break;
	}
}





//------------------------------------------------------------------------------
// main loop

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

	if (SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT, 16, SDL_OPENGL) == NULL)
	{
		printf("Unable to set video mode: %s\n", SDL_GetError());
		exit(EXIT_FAILURE);
	}

	SDL_WM_SetCaption("SDL - OpenGl framework", "OpenGL");

	InitGL();

	// game loop
	bool loopcontinue = true;
	SDL_Event event;

	while (loopcontinue)
	{
		while (SDL_PollEvent(&event))
		{
			switch (event.type)
			{
			case SDL_KEYDOWN:
				if (event.key.keysym.sym == SDLK_ESCAPE)
					loopcontinue = false;
			
				kbd_event(event.key.keysym.sym);	
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

