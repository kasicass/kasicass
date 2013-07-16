#include "KResMgr.h"

// Bin/Debug/xxx.exe => Media/
std::string KResMgr::rootDir_ = "../../Media/";

void KResMgr::SetRootDir(std::string &rootDir)
{
	rootDir_ = rootDir;
}

std::string KResMgr::GetResPath(const std::string &resourceID)
{
	return rootDir_ + resourceID;
}
