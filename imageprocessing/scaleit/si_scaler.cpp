#include "si_scaler.hpp"
#include "si_filter.hpp"
#include "si_ppm.hpp"

namespace si {

Scaler::Scaler(const std::string& infile, float rate, const std::string& filter) :
	infile_(infile), rate_(rate), filter_(filter)
{
}

Scaler::~Scaler()
{
}

void Scaler::run()
{
	ImagePtr img = io::readFromPpm(infile_);
	FILTER_TYPE ftype = FILTER_LINEAR;
	if (filter_ == "linear") ftype = FILTER_LINEAR;
	FilterPtr filter = makeFilter(ftype);
	filter->setInfo(img, rate_);

	int width  = int(rate_ * img->width());
	int height = int(rate_ * img->height());
	ImagePtr outImg = std::make_shared<Image>(width, height);
	for (int y = 0; y < height; ++y)
	{
		for (int x = 0; x < width; ++x)
		{
			outImg->setColor(x, y, filter->pick(x, y));
		}
	}

	io::writeToPpm(outImg, "output.ppm");
}

}

