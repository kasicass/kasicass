
CFLAGS  = -I/usr/local/include `pkg-config --cflags sdl freetype2`
LDFALGS = `pkg-config --libs sdl freetype2`

all:
	gcc -Wall $(CFLAGS) main.c $(LDFALGS)

