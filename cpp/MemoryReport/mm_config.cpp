#include "mm_config.hpp"

namespace mm {

#if defined(MEMORY_REPORT_ENABLE)

static const char* const g_tagNames[] = {
	"mm_system",
	"misc",
	"foo",
	"bar",
	"vector<int>",
	"list<int>",
	"map",
	"buffer",
};

const char* GetTagName(RECORD_TAG tag)
{
	return g_tagNames[tag];
}

#endif

}

