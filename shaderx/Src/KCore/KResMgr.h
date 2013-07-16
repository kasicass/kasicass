#ifndef KCORE_RESMGR_H
#define KCORE_RESMGR_H

#include <string>

class KResMgr
{
public:
	static void SetRootDir(std::string &rootDir);
	static std::string GetResPath(const std::string &resourceID);

private:
	static std::string rootDir_;
};

#endif