// simple fog

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <SDL/SDL.h>
#include <GL/gl.h>
#include <GL/glu.h>


//------------------------------------------------------------------------------
// glut stuff

static GLUquadricObj *quadObj;

#define QUAD_OBJ_INIT() { if(!quadObj) initQuadObj(); }

static void
initQuadObj(void)
{
  quadObj = gluNewQuadric();
}

static void
drawBox(GLfloat size, GLenum type)
{
  static GLfloat n[6][3] =
  {
    {-1.0, 0.0, 0.0},
    {0.0, 1.0, 0.0},
    {1.0, 0.0, 0.0},
    {0.0, -1.0, 0.0},
    {0.0, 0.0, 1.0},
    {0.0, 0.0, -1.0}
  };
  static GLint faces[6][4] =
  {
    {0, 1, 2, 3},
    {3, 2, 6, 7},
    {7, 6, 5, 4},
    {4, 5, 1, 0},
    {5, 6, 2, 1},
    {7, 4, 0, 3}
  };
  GLfloat v[8][3];
  GLint i;

  v[0][0] = v[1][0] = v[2][0] = v[3][0] = -size / 2;
  v[4][0] = v[5][0] = v[6][0] = v[7][0] = size / 2;
  v[0][1] = v[1][1] = v[4][1] = v[5][1] = -size / 2;
  v[2][1] = v[3][1] = v[6][1] = v[7][1] = size / 2;
  v[0][2] = v[3][2] = v[4][2] = v[7][2] = -size / 2;
  v[1][2] = v[2][2] = v[5][2] = v[6][2] = size / 2;

  for (i = 5; i >= 0; i--) {
    glBegin(type);
    glNormal3fv(&n[i][0]);
    glVertex3fv(&v[faces[i][0]][0]);
    glVertex3fv(&v[faces[i][1]][0]);
    glVertex3fv(&v[faces[i][2]][0]);
    glVertex3fv(&v[faces[i][3]][0]);
    glEnd();
  }
}

void
k_glutSolidCube(GLdouble size)
{
  drawBox(size, GL_QUADS);
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




//------------------------------------------------------------------------------
// my demo

#define	SCREEN_WIDTH	640
#define	SCREEN_HEIGHT	480

static GLint fog_mode;

//
// Init GL
//
void InitGL(void)
{
	// set camera
	glViewport(0, 0, (GLint)SCREEN_WIDTH, (GLint)SCREEN_HEIGHT);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	if ( SCREEN_WIDTH <= SCREEN_HEIGHT )
	{
		glOrtho(-2.5, 2.5, -2.5*(GLfloat)SCREEN_HEIGHT/(GLfloat)SCREEN_WIDTH,
			2.5*(GLfloat)SCREEN_HEIGHT/(GLfloat)SCREEN_WIDTH, -10.0, 10.0);
	}
	else
	{
		glOrtho(-2.5*(GLfloat)SCREEN_WIDTH/(GLfloat)SCREEN_HEIGHT,
			2.5*(GLfloat)SCREEN_WIDTH/(GLfloat)SCREEN_HEIGHT, -2.5, 2.5, -10.0, 10.0);
	}

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();


	// light
	GLfloat position[] = { 0.5, 0.5, 3.0, 0.0 };

	glEnable(GL_DEPTH_TEST);

	glLightfv(GL_LIGHT0, GL_POSITION, position);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);

	GLfloat mat_ambient[] = { 0.17425, 0.01175, 0.01175 };
	GLfloat	mat_diffuse[] = { 0.61424, 0.04136, 0.04136 };
	GLfloat mat_specular[] = { 0.727811, 0.626959, 0.626959 };
	glMaterialfv(GL_FRONT, GL_AMBIENT, mat_ambient);
	glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_diffuse);
	glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
	glMaterialf(GL_FRONT, GL_SHININESS, 0.6*128.0);

	// fog
	glEnable(GL_FOG);
	GLfloat fog_color[] = { 0.5, 0.5, 0.5, 1.0 };

	fog_mode = GL_EXP;
	glFogi(GL_FOG_MODE, fog_mode);
	glFogfv(GL_FOG_COLOR, fog_color);
	glFogf(GL_FOG_DENSITY, 0.35);
	glHint(GL_FOG_HINT, GL_DONT_CARE);
	glFogf(GL_FOG_START, 1.0);
	glFogf(GL_FOG_END, 5.0);

	glClearColor(0.5, 0.5, 0.5, 1.0);
}


//
// To render the OpenGL scene
//
void RenderSphere(GLfloat x, GLfloat y, GLfloat z)
{
	glPushMatrix();
		glTranslatef(x, y, z);
		k_glutSolidSphere(0.4, 16, 16);
	glPopMatrix();
}

void RenderScene(void)
{
	// 
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	RenderSphere(-2.0, -0.5, -1.0);
	RenderSphere(-1.0, -0.5, -2.0);
	RenderSphere(0.0, -0.5, -3.0);
	RenderSphere(1.0, -0.5, -4.0);
	RenderSphere(2.0, -0.5, -5.0);

	glFlush();
}

void kbd_event(int sym)
{
	switch (sym)
	{
	case SDLK_f:
		if (fog_mode == GL_EXP) { fog_mode = GL_EXP2; printf("fog mode: GL_EXP2\n"); }
		else if ( fog_mode == GL_EXP2 ) { fog_mode = GL_LINEAR; printf("fog mode: GL_LINEAR\n"); }
		else if ( fog_mode == GL_LINEAR ) { fog_mode = GL_EXP; printf("fog mode: GL_EXP\n"); }
		glFogi(GL_FOG_MODE, fog_mode);
		break;

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

