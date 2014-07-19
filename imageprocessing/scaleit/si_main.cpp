#include "si_scaler.hpp"
#include <iostream>

void usage()
{
	std::cout << "usage:\n"
						<< "  scaleit.exe <in_ppm_file> <out_ppm_file> <scale_rate> [filter]\n"
						<< "\n"
						<< "  filter = linear|bilinear|...\n"
						<< "\n"
						<< "eg:\n"
						<< "  scaleit.exe a.ppm output.ppm 1.7\n"
						<< "  scaleit.exe a.ppm output.ppm 0.5 linear" << std::endl;
}

int main(int argc, char* argv[])
{
	if (argc != 4 && argc != 5)
	{
		usage();
		return 0;
	}

	std::string filter = "linear";
	if (argc == 5)
		filter = argv[4];

	float rate = std::atof(argv[3]);
	si::Scaler scaler(argv[1], argv[2], rate, filter);
	scaler.run();
	
	return 0;
}

