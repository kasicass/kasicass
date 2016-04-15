#include <windows.h>													// Header File For Windows
#include <gl/glew.h>
#include <gl/gl.h>														// Header File For The OpenGL32 Library
#include <gl/glu.h>														// Header File For The GLu32 Library

#include <stdio.h>
#include "NeHeGL.h"														// Header File For NeHeGL

#pragma comment( lib, "opengl32.lib" )									// Search For OpenGL32.lib While Linking
#pragma comment( lib, "glu32.lib" )										// Search For GLu32.lib While Linking
#pragma comment( lib, "glew32.lib" )

#define		TWO_PI 6.2831853071											// PI * 2

GL_Window*	g_window;													// Window Structure
Keys*		g_keys;														// Keyboard

GLuint g_shader, g_fragment, g_program;


const GLchar* shaderSrc[] = {
	"void main()"
	"{"
	"	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;"
	"}"
};

const GLchar* fragSrc[] = {
	"void main(void)"
	"{"
		"gl_FragColor = vec4( 0.0, 1.0, 0.0, 1.0 );"
	"}"
};

BOOL InitGLSL()
{
	GLint compiled, linked;

	g_shader = glCreateShader(GL_VERTEX_SHADER);
	
	glShaderSource(g_shader, 1, shaderSrc, NULL);
	glCompileShader(g_shader);

	glGetShaderiv(g_shader, GL_COMPILE_STATUS, &compiled);
	if ( !compiled )
	{
		puts("111");
		return FALSE;
	}

	g_fragment = glCreateShader(GL_FRAGMENT_SHADER);

	glShaderSource(g_fragment, 1, fragSrc, NULL);
	glCompileShader(g_fragment);

	glGetShaderiv(g_fragment, GL_COMPILE_STATUS, &compiled);
	if ( !compiled )
	{
		puts("222");
		return FALSE;
	}

	g_program = glCreateProgram();
	glAttachShader(g_program, g_shader);
	glAttachShader(g_program, g_fragment);
	glLinkProgram(g_program);

	glGetProgramiv(g_program, GL_LINK_STATUS, &linked);
	if ( !linked )
	{
		puts("333");
		return FALSE;
	}

	glUseProgram(g_program);
	return TRUE;
}

BOOL Initialize (GL_Window* window, Keys* keys)							// Any GL Init Code & User Initialiazation Goes Here
{
	g_window	= window;												// Window Values
	g_keys		= keys;													// Key Values

	// Start Of User Initialization
	glClearColor (0.0f, 0.0f, 0.0f, 0.5f);								// Black Background
	glClearDepth (1.0f);												// Depth Buffer Setup
	glEnable (GL_DEPTH_TEST);											// Enable Depth Testing
	glShadeModel (GL_SMOOTH);											// Select Smooth Shading
	glHint (GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);					// Set Perspective Calculations To Most Accurate

	puts("k 111");
	glewInit();
	if ( !glewIsSupported("GL_VERSION_2_0") )
		return FALSE;

	puts("k 222");
	if ( !InitGLSL() )
		return FALSE;

	puts("k 333");
	return TRUE;														// Return TRUE (Initialization Successful)
}

void Deinitialize (void)												// Any User DeInitialization Goes Here
{
}

void Update (float milliseconds)										// Perform Motion Updates Here
{
	if (g_keys->keyDown [VK_ESCAPE])									// Is ESC Being Pressed?
		TerminateApplication (g_window);								// Terminate The Program
}

void Draw (void)
{
	glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);				// Clear Screen And Depth Buffer

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();													// Reset The Modelview Matrix

	// Position The Camera To Look At Our Mesh From A Distance
	gluLookAt(0.0f, 0.0f, -10.0f, 0.0f, 0.0f, 0.0f, 0, 1, 0);

	glBegin(GL_TRIANGLES);
	  glColor3f(1.0f, 0.0f, 0.0f);
	  glVertex3f(0.0f, 1.0f, 0.0f);
	  glVertex3f(-1.0f, -1.0f, 0.0f);
	  glVertex3f(1.0f, -1.0f, 0.0f);
	glEnd();

	glFlush ();															// Flush The GL Rendering Pipeline
}
