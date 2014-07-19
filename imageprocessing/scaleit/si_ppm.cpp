// ppm format:
// http://en.wikipedia.org/wiki/Netpbm_format

#include "si_ppm.hpp"
#include <iostream>
#include <fstream>
#include <sstream>
#include <assert.h>

namespace si { namespace io {

ImagePtr readFromPpm(const std::string& filename)
{
	std::string line;
	std::ifstream fin(filename);

	// magic number
	std::getline(fin, line);
	assert(line == "P3");

	enum PPM_STATE { PPM_WIDTH, PPM_HEIGHT, PPM_MAXCOLOR, PPM_RED, PPM_GREEN, PPM_BLUE };
	PPM_STATE state = PPM_WIDTH;

	int token;
	int red, green, blue;
	int width, height;
	int x = 0, y = 0;
	ImagePtr img;
	while (fin.good())
	{
		std::getline(fin, line);
		if (line.size() == 0 || line[0] == '#')
			continue;

		std::istringstream ss(line);
		while (ss >> token)
		{
			switch (state)
			{
			case PPM_WIDTH:
				width = token;
				state = PPM_HEIGHT;
				break;

			case PPM_HEIGHT:
				height = token;
				state = PPM_MAXCOLOR;
				img = std::make_shared<Image>(width, height);
				break;

			case PPM_MAXCOLOR:
				assert(token == 255);
				state = PPM_RED;
				break;

			case PPM_RED:
				red = token;
				state = PPM_GREEN;
				break;

			case PPM_GREEN:
				green = token;
				state = PPM_BLUE;
				break;

			case PPM_BLUE:
				blue = token;
				state = PPM_RED;
				img->setColor(x, y, Color(red, green, blue));
				x += 1;
				if (x == width) { x = 0; y += 1; }
				break;
			}
		}
	}

	assert(state == PPM_RED);
	assert(x == 0);
	assert(y == height);
	return img;
}

void writeToPpm(ImagePtr img, const std::string& filename)
{
	std::ofstream fout(filename);
	fout << "P3" << std::endl;
	fout << "# Kasicass' ScaleIt PPM Writer" << std::endl;
	fout << img->width() << " " << img->height() << std::endl;
	fout << "255" << std::endl; // max color
	
	int x, y;
	for (y = 0; y < img->height(); ++y)
	{
		for (x = 0; x < img->width(); ++x)
		{
			Color c = img->getColor(x, y);
			fout << c.r << " " << c.g << " " << c.b << std::endl;
		}
	}
}

}}

