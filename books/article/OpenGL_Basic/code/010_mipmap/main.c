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

GLubyte mipmap32[32][32][4];
GLubyte mipmap16[16][16][4];
GLubyte mipmap8[8][8][4];
GLubyte mipmap4[4][4][4];
GLubyte mipmap2[2][2][2];
GLubyte mipmap1[1][1][4]; 

GLuint texName;

//
// Init GL
//
void InitTexture(void)
{
	int i, j;

	for ( i = 0; i < 32; i++ )
	{
		for ( j = 0; j < 32; j++ )
		{
			mipmap32[i][j][0] = 255;
			mipmap32[i][j][1] = 255;
			mipmap32[i][j][2] = 0;
			mipmap32[i][j][3] = 255;
		}
	}

	for ( i = 0; i < 16; i++ )
	{
		for ( j = 0; j < 16; j++ )
		{
			mipmap16[i][j][0] = 255;
			mipmap16[i][j][1] = 0;
			mipmap16[i][j][2] = 255;
			mipmap16[i][j][3] = 255;
		}
	}

	for ( i = 0; i < 8; i++ )
	{
		for ( j = 0; j < 8; j++ )
		{
			mipmap8[i][j][0] = 255;
			mipmap8[i][j][1] = 0;
			mipmap8[i][j][2] = 0;
			mipmap8[i][j][3] = 255;
		}
	}

	for ( i = 0; i < 4; i++ )
	{
		for ( j = 0; j < 4; j++ )
		{
			mipmap4[i][j][0] = 0;
			mipmap4[i][j][1] = 255;
			mipmap4[i][j][2] = 0;
			mipmap4[i][j][3] = 255;
		}
	}

	for ( i = 0; i < 2; i++ )
	{
		for ( j = 0; j < 2; j++ )
		{
			mipmap2[i][j][0] = 0;
			mipmap2[i][j][1] = 0;
			mipmap2[i][j][2] = 255;
			mipmap2[i][j][3] = 255;
		}
	}

	mipmap1[i][j][0] = 255;
	mipmap1[i][j][1] = 255;
	mipmap1[i][j][2] = 255;
	mipmap1[i][j][3] = 255;
}


void InitGL(void)
{
	// set camera
	glViewport(0, 0, (GLsizei) SCREEN_WIDTH, (GLsizei) SCREEN_HEIGHT);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(60.0, (GLfloat) SCREEN_WIDTH / (GLfloat) SCREEN_HEIGHT, 1.0, 7000.0);
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
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_NEAREST);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, mipmap32);
	glTexImage2D(GL_TEXTURE_2D, 1, GL_RGBA, 16, 16, 0, GL_RGBA, GL_UNSIGNED_BYTE, mipmap16);
	glTexImage2D(GL_TEXTURE_2D, 2, GL_RGBA, 8, 8, 0, GL_RGBA, GL_UNSIGNED_BYTE, mipmap8);
	glTexImage2D(GL_TEXTURE_2D, 3, GL_RGBA, 4, 4, 0, GL_RGBA, GL_UNSIGNED_BYTE, mipmap4);
	glTexImage2D(GL_TEXTURE_2D, 4, GL_RGBA, 2, 2, 0, GL_RGBA, GL_UNSIGNED_BYTE, mipmap2);
	glTexImage2D(GL_TEXTURE_2D, 5, GL_RGBA, 1, 1, 0, GL_RGBA, GL_UNSIGNED_BYTE, mipmap1);
}

void RenderScene(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glEnable(GL_TEXTURE_2D);
	glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
	glBindTexture(GL_TEXTURE_2D, texName);

	glBegin(GL_QUADS);
		glTexCoord2f(0.0, 0.0);		glVertex3f(-2.0, -1.0, 0.0);
		glTexCoord2f(0.0, 1.0);		glVertex3f(-2.0, 1.0, 0.0);
		glTexCoord2f(1.0, 1.0);		glVertex3f(2000.0, 1.0, -6000.0);
		glTexCoord2f(1.0, 0.0);		glVertex3f(2000.0, -1.0, -6000.0);
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

