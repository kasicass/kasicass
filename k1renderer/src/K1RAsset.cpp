#include "K1RAsset.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

namespace K1R {

//
// File
//
File::File(unsigned int len) : length_(len), data_(nullptr)
{
	data_ = malloc(len);
}

File::~File()
{
	if (data_)
	{
		free(data_);
		data_ = nullptr;
	}
}

void *File::data()
{
	return data_;
}

unsigned int File::length()
{
	return length_;
}


//
// Asset
//
namespace {
std::string s_rootPath = "../../data/";
}

void Asset::setRootDir(const std::string &rootPath)
{
	s_rootPath = rootPath;
}

FilePtr Asset::readFile(const std::string &resourceID)
{
	std::string fileName = s_rootPath + resourceID;
	FILE *fp = fopen(fileName.c_str(), "rb");
	assert(fp != NULL);

	fseek(fp, 0, SEEK_END);
	long fileLen = ftell(fp);

	FilePtr pFile(new File(static_cast<unsigned int>(fileLen)));
	fseek(fp, 0, SEEK_SET);
	fread(pFile->data(), fileLen, 1, fp);
	fclose(fp);

	return pFile;
}

}
