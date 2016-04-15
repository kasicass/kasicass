// cube mapping texture

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

static GLubyte mytex1[TEX_HEIGHT][TEX_WIDTH][4];
static GLubyte mytex2[TEX_HEIGHT][TEX_WIDTH][4];
static GLubyte mytex3[TEX_HEIGHT][TEX_WIDTH][4];
static GLubyte mytex4[TEX_HEIGHT][TEX_WIDTH][4];
static GLubyte mytex5[TEX_HEIGHT][TEX_WIDTH][4];
static GLubyte mytex6[TEX_HEIGHT][TEX_WIDTH][4];
static GLuint  texName;


static GLUquadricObj *quadObj;

#define QUAD_OBJ_INIT() { if(!quadObj) initQuadObj(); }

static void
initQuadObj(void)
{
  quadObj = gluNewQuadric();
}

void
k_glutSolidSphere(GLdouble radius, GLint slices, GLint stacks)
{
  QUAD_OBJ_INIT();
  gluQuadricDrawStyle(quadObj, GLU_FILL);
  gluQuadricNormals(quadObj, GLU_SMOOTH);
  /* If we ever changed/used the texture or orientation state
 *      of quadObj, we'd need to change it to the defaults here
 *           with gluQuadricTexture and/or gluQuadricOrientation. */
  gluSphere(quadObj, radius, slices, stacks);
}

//
// Init GL
//
void InitTexture(void)
{
	int i, j;

	for ( i = 0; i < TEX_HEIGHT; i++ )
	{
		for ( j = 0; j < TEX_WIDTH; j++ )
		{
			mytex1[i][j][0] = 255;
			mytex1[i][j][1] = 0;
			mytex1[i][j][2] = 0;
			mytex1[i][j][3] = 255;
		}
	}

	for ( i = 0; i < TEX_HEIGHT; i++ )
	{
		for ( j = 0; j < TEX_WIDTH; j++ )
		{
			mytex2[i][j][0] = 0;
			mytex2[i][j][1] = 255;
			mytex2[i][j][2] = 0;
			mytex2[i][j][3] = 255;
		}
	}

	for ( i = 0; i < TEX_HEIGHT; i++ )
	{
		for ( j = 0; j < TEX_WIDTH; j++ )
		{
			mytex3[i][j][0] = 0;
			mytex3[i][j][1] = 0;
			mytex3[i][j][2] = 255;
			mytex3[i][j][3] = 255;
		}
	}

	for ( i = 0; i < TEX_HEIGHT; i++ )
	{
		for ( j = 0; j < TEX_WIDTH; j++ )
		{
			mytex4[i][j][0] = 255;
			mytex4[i][j][1] = 255;
			mytex4[i][j][2] = 0;
			mytex4[i][j][3] = 255;
		}
	}

	for ( i = 0; i < TEX_HEIGHT; i++ )
	{
		for ( j = 0; j < TEX_WIDTH; j++ )
		{
			mytex5[i][j][0] = 255;
			mytex5[i][j][1] = 0;
			mytex5[i][j][2] = 255;
			mytex5[i][j][3] = 255;
		}
	}

	for ( i = 0; i < TEX_HEIGHT; i++ )
	{
		for ( j = 0; j < TEX_WIDTH; j++ )
		{
			mytex6[i][j][0] = 255;
			mytex6[i][j][1] = 255;
			mytex6[i][j][2] = 255;
			mytex6[i][j][3] = 255;
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
	glTranslatef(0.0, 0.0, -4);

	glClearColor(0.5, 0.5, 0.5, 0.0);
	glShadeModel(GL_FLAT);
	glEnable(GL_DEPTH_TEST);

	InitTexture();
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

	glGenTextures(1, &texName);
	glBindTexture(GL_TEXTURE_CUBE_MAP, texName);

	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

	glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X, 0, GL_RGBA, TEX_WIDTH, TEX_HEIGHT, 0, GL_RGBA, GL_UNSIGNED_BYTE, mytex1);
	glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X, 0, GL_RGBA, TEX_WIDTH, TEX_HEIGHT, 0, GL_RGBA, GL_UNSIGNED_BYTE, mytex2);
	glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y, 0, GL_RGBA, TEX_WIDTH, TEX_HEIGHT, 0, GL_RGBA, GL_UNSIGNED_BYTE, mytex3);
	glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, 0, GL_RGBA, TEX_WIDTH, TEX_HEIGHT, 0, GL_RGBA, GL_UNSIGNED_BYTE, mytex4);
	glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z, 0, GL_RGBA, TEX_WIDTH, TEX_HEIGHT, 0, GL_RGBA, GL_UNSIGNED_BYTE, mytex5);
	glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, 0, GL_RGBA, TEX_WIDTH, TEX_HEIGHT, 0, GL_RGBA, GL_UNSIGNED_BYTE, mytex6);

	glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
	glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
	glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP);
	glEnable(GL_TEXTURE_GEN_S);
	glEnable(GL_TEXTURE_GEN_T);
	glEnable(GL_TEXTURE_GEN_R);
}

void RenderScene(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glEnable(GL_TEXTURE_CUBE_MAP);
	glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
	glBindTexture(GL_TEXTURE_CUBE_MAP, texName);

	k_glutSolidSphere(1.0, 30, 10);

/*	
	glBegin(GL_QUADS);
		glVertex3f(-1.0, -1.0, -1.0);
		glVertex3f(-1.0,  1.0, -1.0);
		glVertex3f( 1.0,  1.0, -1.0);
		glVertex3f( 1.0, -1.0, -1.0);
	glEnd();
*/

	glFlush();
	glDisable(GL_TEXTURE_CUBE_MAP);
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

