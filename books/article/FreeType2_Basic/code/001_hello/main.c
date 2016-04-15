/**
 * FreeType2 helloworld with SDL
 * font-file: wenq.org
 */

#include <SDL.h>
#include <stdio.h>
#include <stdlib.h>

#include <ft2build.h>
#include FT_FREETYPE_H


#define	WIDTH		640
#define	HEIGHT		480

#define	START_X		10
#define	START_Y		400

#define	FONT_FILE	"wqy-zenhei/wqy-zenhei.ttf"

FT_Face face;
FT_GlyphSlot  slot;

Uint16 CreateColorPixel( SDL_PixelFormat *fmt,
		Uint8 red, Uint8 green, Uint8 blue )
{
	Uint16 value;
	// This series of bit shifts uses the information
	// from the SDL_Format structure to correctly compose
	// a 16-bit pixel value from 8-bit red, green and
	// blue data

	value = ((red >> fmt->Rloss) << fmt->Rshift) +
		((green >> fmt->Gloss) << fmt->Gshift) +
		((blue >> fmt->Bloss) << fmt->Bshift);

	return value;
}

int InitFreeType2()
{
#define	ERRCODE(n) if (error<0) return n;
	FT_Library library;
	FT_Error   error;

	error = FT_Init_FreeType( &library );
	ERRCODE(-1);

	error = FT_New_Face( library, FONT_FILE, 0, &face );
	ERRCODE(-2);
	
	// use 18pt at 300dpi	
	error = FT_Set_Char_Size(face, 18*64, 0, 300, 0);
	ERRCODE(-3);

	slot = face->glyph;
	return 0;
}

void DrawBitmap(SDL_Surface *screen, FT_Bitmap *bitmap, FT_Int x, FT_Int y)
{
	Uint16 * raw_pixels;
	FT_Int i, j, p, q;
	FT_Int x_max = x + bitmap->width;
	FT_Int y_max = y + bitmap->rows;

	SDL_LockSurface(screen);
	raw_pixels = (Uint16 *)screen->pixels;
	for ( i = x, p = 0; i < x_max; i++, p++ )
	{
		for ( j = y, q = 0; j < y_max; j++, q++ )
		{
			int offset;

			if ( i < 0 || j < 0 || i >= WIDTH || j>= HEIGHT )
				continue;

			offset = (screen->pitch / 2 * j + i);
			raw_pixels[offset] |= bitmap->buffer[q*bitmap->width+p];
		}
	}	
	SDL_UnlockSurface(screen);
}

void DrawText(SDL_Surface * screen)
{
	#define _NUM_ELEMS(arr) sizeof(arr)/sizeof(arr[0])

	int i;
	FT_Vector pen;
	FT_Error error;
	FT_ULong text[] = { 0x4e2d, 0x61, 0x6587, 0x62 };

	pen.x = START_X * 64;
	pen.y = (HEIGHT - START_Y) * 64;

	for ( i = 0; i < _NUM_ELEMS(text); i++ )
	{
		FT_Set_Transform(face, NULL, &pen);

		error = FT_Load_Char(face, text[i], FT_LOAD_RENDER);
		if ( error < 0 ) continue;

		DrawBitmap(screen, &slot->bitmap, slot->bitmap_left, 
			HEIGHT - slot->bitmap_top);

		pen.x += slot->advance.x;
		pen.y += slot->advance.y;
	}
}

int main( int argc, char * argv[] )
{
	SDL_Surface * screen;
	int ret;
	int loopcontinue = 1;
	SDL_Event event;


	// Initialise SDL's video system and check for errors
	if ( SDL_Init(SDL_INIT_VIDEO) != 0 )
	{
		printf( "Unable to initialise SDL: %s\n", SDL_GetError() );
		exit( EXIT_FAILURE );
	}

	// Make sure SDL_Quit gets called when the program exits
	atexit( SDL_Quit );

	// Attempt to set a 256x256 (16-bit) video mode. This
	// will set some type of 16-bit mode, but we won't know
	// which particular pixel format ahead of time. If the
	// video card can't handle certain modes, SDL will emulate it
	screen = SDL_SetVideoMode( WIDTH, HEIGHT, 16, 0 );

	if ( screen == NULL )
	{
		printf( "Unable to set video mode: %s\n", SDL_GetError() );
		exit( EXIT_FAILURE );
	}

	// Get a pointer to the video surface's memory
	if ( (ret = InitFreeType2()) < 0 )
	{
		printf( "FT Init fail: %d\n", ret );
		exit( EXIT_FAILURE );
	}
	DrawText(screen);
	

	// Inform SDL that the screen has been changed. This is necessary
	// because SDL's screen surface is not always the real framebuffer
	// it is sometimes emulated behind the scenes

	SDL_UpdateRect(screen, 0, 0, 0, 0);

	// wait for event, quit when user closes the
	// window or press ESC
	while ( loopcontinue )
	{
		if ( SDL_WaitEvent(&event) == 0 )
		{
			break;
		}

		switch ( event.type )
		{
			case SDL_KEYDOWN:
				if ( event.key.keysym.sym == SDLK_ESCAPE )
				{
					// press ESC to quit
					loopcontinue = 0;
				}
				break;

			case SDL_QUIT: // window closed, quit
				loopcontinue = 0;
				break;
		}
	}

	return EXIT_SUCCESS;
} 
