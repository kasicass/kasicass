#pragma once

class DumpWorker
{
public:
	enum DUMP_TYPE { FULL_DUMP, MINI_DUMP };

public:
	static void Init(const char *dumpFilename, DUMP_TYPE dtype);
};
