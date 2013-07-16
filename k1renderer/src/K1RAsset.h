#ifndef K1R_UTIL_ASSET_H
#define K1R_UTIL_ASSET_H

// K1R::Asset::setRootDir("../../asset/");
// K1R::Asset::readFile("shaders/color.vsh");

#include <string>
#include <memory>

namespace K1R {

class File
{
public:
	File(unsigned int len);
	~File();

	void *data();
	unsigned int length();

private:
	void *data_;
	unsigned int length_;
};
typedef std::shared_ptr<File> FilePtr;

class Asset
{
public:
	static void setRootDir(const std::string &rootPath);
	static FilePtr readFile(const std::string &resourceID);
};

}

#endif