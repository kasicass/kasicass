#ifndef K1R_RENDER_COLOR_H
#define K1R_RENDER_COLOR_H

namespace K1R {

typedef unsigned int Color;
inline Color RGBA(unsigned int r, unsigned int g, unsigned int b, unsigned int a)
{
	return ((a&0xFF)<<24) | ((r&0xFF)<<16) | ((g&0xFF)<<8) | (b&0xFF);
}

}

#endif