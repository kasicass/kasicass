// super simple planar shadow
// http://www.csie.ntu.edu.tw/~r89004/hive/shadow/page_1.html

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <SDL/SDL.h>
#include <GL/gl.h>
#include <GL/glu.h>



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
void InitGL(void)
{
	glClearColor(0, 0, 0, 0);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(90, 640.0f/480.0f, 1.0, 50.0);
	glViewport(0, 0, 640, 480);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(0.0, 0.0, 3.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);	

	glEnable(GL_BLEND);
	printf("vendor: %s\n", glGetString(GL_VENDOR));

	// lights
	GLfloat mat_specular[] = { 1.0, 1.0, 1.0, 1.0 };
	GLfloat mat_diffuse[] = { 0.8, 0.8, 0.8, 1.0 };
//	GLfloat mat_ambient[] = { 0.0, 0.8, 0.8, 1.0 };
	GLfloat mat_shininess[] = { 50.0 };
	GLfloat light_position[] = { 1.0, 1.0, 3.0, 1.0 };
	GLfloat white_light[] = { 1.0, 1.0, 1.0, 1.0 };
	GLfloat lmodel_ambient[] = { 0.5, 0.5, 0.5, 1.0 };

	glShadeModel(GL_SMOOTH);
	glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
	glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_diffuse);
//	glMaterialfv(GL_FRONT, GL_AMBIENT, mat_ambient);
	glMaterialfv(GL_FRONT, GL_SHININESS, mat_shininess);

	glLightfv(GL_LIGHT0, GL_POSITION, light_position);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, white_light);
	glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lmodel_ambient);
	// glLightModelf(GL_LIGHT_MODEL_LOCAL_VIEWER, 0.0);

	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glEnable(GL_DEPTH_TEST);
}

//
// To render the OpenGL scene
//
void RenderScene(void)
{
	static GLfloat camera_y = 0.0;
	static GLfloat dy = 0.01;

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	camera_y += dy;
	if ( camera_y >= 2.0 ) dy = -dy;
	else if ( camera_y <= -2.0 ) dy = -dy;

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(0.0, camera_y, 3.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);	

	// background
	GLfloat r = 3.0f;
	glBegin(GL_QUADS);
		glColor3f(1.0, 1.0, 1.0);
		glVertex3f(-r, r, -1.0);
		glVertex3f(-r, -r, -1.0);
		glVertex3f(r, -r, -1.0);
		glVertex3f(r, r, -1.0);
	glEnd();

/*
	// front body
	glBegin(GL_TRIANGLES);
		glColor3f(1.0, 0, 0);
		glVertex3f(0.0, 1.0, 0.0);
		glColor3f(0, 1.0, 0);
		glVertex3f(1.0, -1.0, 0.0);
		glColor3f(0, 0, 1.0);
		glVertex3f(-1.0, -1.0, 0.0);
	glEnd();
*/
	k_glutSolidSphere(1.0, 20, 16);

	// shadow
	// L = { 0, 0, -1, 0 }
	// P = { 0, 0, 1, 1 }	// z = -1
	GLfloat m[] = {
		1.0, 0.0, 0.0, 0.0,
		0.0, 1.0, 0.0, 0.0,
		-0.5, 0.5, 0.0, 0.0,
		-0.5, 0.5, -0.99, 1.0,
	};
	glPushMatrix();
		glDisable(GL_LIGHTING);

		glMatrixMode(GL_MODELVIEW);
		glMultMatrixf(m);

//		glBegin(GL_TRIANGLES);
//			glColor3f(1.0, 0, 0);
//			glVertex3f(0.0, 1.0, 0.0);
//			glVertex3f(1.0, -1.0, 0.0);
//			glVertex3f(-1.0, -1.0, 0.0);
//		glEnd();

			k_glutSolidSphere(1.0, 20, 16);
		glEnable(GL_LIGHTING);
	glPopMatrix();
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

