
INCLUDE = -I/usr/local/include `pkg-config --cflags sdl`
LDLIBS  = -lglut -lXext -lX11 -lXmu -lXi -lGL -lGLU -lm
LDFLAGS = -L/usr/X11R6/lib `pkg-config --libs sdl`

all:
	gcc -std=c99 -Wall $(INCLUDE) main.c mymd2.c $(LDFLAGS) $(LDLIBS)

