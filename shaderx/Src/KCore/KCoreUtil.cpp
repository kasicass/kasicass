#include "KCoreUtil.h"
#include <stdio.h>

bool KCReadFileContent(const std::string &fileName, char* buf, long bufSize)
{
	FILE *fp = fopen(fileName.c_str(), "rb");
	fseek(fp, 0, SEEK_END);
	long fileLen = ftell(fp);
	if (fileLen > bufSize-1)
	{
		fclose(fp);
		return false;
	}

	fseek(fp, 0, SEEK_SET);
	fread(buf, fileLen, 1, fp);
	fclose(fp);

	buf[fileLen] = '\0';
	return true;
}
