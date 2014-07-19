// scaleit.exe <ppm_file> <scale_rate>
// eg. scaleit.exe a.ppm 1.7

#include "si_ppm.hpp"

int main(int argc, char* argv[])
{
	si::ImagePtr img = si::io::readFromPpm("test.ppm");
	si::io::writeToPpm(img, "output.ppm");
	return 0;
}

