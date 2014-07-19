// scaleit.exe <ppm_file> <scale_rate> [filter]
// eg.
//   $ scaleit.exe a.ppm 1.7
//   $ scaleit.exe a.ppm 0.5 linear

#include "si_scaler.hpp"
#include <iostream>

void usage()
{
	std::cout << "usage:\n"
						<< "  scaleit.exe <ppm_file> <scale_rate> [filter]\n"
						<< "\n"
						<< "  filter = linear|...\n"
						<< "\n"
						<< "eg:\n"
						<< "  scaleit.exe a.ppm 1.7\n"
						<< "  scaleit.exe a.ppm 0.5 linear" << std::endl;
}

int main(int argc, char* argv[])
{
	if (argc != 3 && argc != 4)
	{
		usage();
		return 0;
	}

	std::string filter = "linear";
	if (argc == 4)
		filter = argv[3];

	float rate = std::atof(argv[2]);
	si::Scaler scaler(argv[1], rate, filter);
	scaler.run();
	
	return 0;
}

