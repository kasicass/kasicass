#pragma once

#include <string>

namespace si {

class Scaler
{
public:
	Scaler(const std::string& infile, const std::string& outfile, float rate, const std::string& ftype);
	~Scaler();

	void run();

private:
	std::string infile_;
	std::string outfile_;
	float rate_;
	std::string ftype_;
};

}

