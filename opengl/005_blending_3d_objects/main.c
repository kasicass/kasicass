// blending 3D object

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

#define	MAXZ			1.0
#define	MINZ			-1.0
#define	ZINC			0.001
static float transparentZ = MINZ;
static float zinc = ZINC;
static GLuint sphereList, cubeList;

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
		glOrtho(-1.5, 1.5, -1.5*(GLfloat)SCREEN_HEIGHT/(GLfloat)SCREEN_WIDTH,
			1.5*(GLfloat)SCREEN_HEIGHT/(GLfloat)SCREEN_WIDTH, -10.0, 10.0);
	}
	else
	{
		glOrtho(-1.5*(GLfloat)SCREEN_WIDTH/(GLfloat)SCREEN_HEIGHT,
			1.5*(GLfloat)SCREEN_WIDTH/(GLfloat)SCREEN_HEIGHT, -1.5, 1.5, -10.0, 10.0);
	}

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();


	// lights
	GLfloat mat_specular[] = { 1.0, 1.0, 1.0, 0.15 };
	GLfloat mat_shininess[] = { 100.0 };
	GLfloat position[] = { 0.5, 0.5, 1.0, 0.0 };

	glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
	glMaterialfv(GL_FRONT, GL_SHININESS, mat_shininess);
	glLightfv(GL_LIGHT0, GL_POSITION, position);

	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glEnable(GL_DEPTH_TEST);

	// object
	sphereList = glGenLists(1);
	glNewList(sphereList, GL_COMPILE);
		k_glutSolidSphere(0.5, 16, 16);
	glEndList();

	cubeList = glGenLists(1);
	glNewList(cubeList, GL_COMPILE);
		k_glutSolidCube(0.6);
	glEndList();
}


//
// To render the OpenGL scene
//
void RenderScene(void)
{
	GLfloat mat_solid[] = { 0.75, 0.75, 0.0, 1.0 };
	GLfloat mat_zero[] = { 0.0, 0.0, 0.0, 1.0 };
	GLfloat mat_transparent[] = { 0.0, 0.8, 0.8, 0.6 };
	GLfloat mat_emission[] = { 0.0, 0.3, 0.3, 0.6 };
	GLfloat mat_ambient[] = { 0.0, 0.5, 0.0, 1.0 };

	//
	transparentZ += zinc;
	if (transparentZ < MINZ || transparentZ > MAXZ)
		zinc = -zinc;

	// 
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glPushMatrix();
		glTranslatef(-0.15, -0.15, 0.0);
		glMaterialfv(GL_FRONT, GL_EMISSION, mat_zero);
		glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_solid);
		glMaterialfv(GL_FRONT, GL_AMBIENT, mat_ambient);
		glCallList(sphereList);
	glPopMatrix();

	glPushMatrix();
		glTranslatef(0.15, 0.15, transparentZ);
		glRotatef(15.0, 1.0, 1.0, 0.0);
		glRotatef(30.0, 0.0, 1.0, 0.0);
		glMaterialfv(GL_FRONT, GL_EMISSION, mat_emission);
		glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_transparent);
		glEnable(GL_BLEND);
		glDepthMask(GL_FALSE);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE);
		glCallList(cubeList);
		glDepthMask(GL_TRUE);
		glDisable(GL_BLEND);
	glPopMatrix();
}

void kbd_event(int sym)
{
	switch (sym)
	{
	case SDLK_UP:
		if (transparentZ < MAXZ) transparentZ += ZINC;
		break;

	case SDLK_DOWN:
		if (transparentZ > MINZ) transparentZ -= ZINC;
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

