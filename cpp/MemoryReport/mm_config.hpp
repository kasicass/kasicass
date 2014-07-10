#pragma once

namespace mm {

enum RECORD_TAG {
	TAG_MM_SELF = 0,   // MemoryReport self memory usage
	TAG_MISC,
	TAG_FOO,
	TAG_BAR,
	TAG_VECTOR_INT,
	TAG_LIST_INT,
	TAG_MAP,
	TAG_BUFFER,

	TAG_VECTOR_A,
	TAG_VECTOR_B,

	TAG_COUNT,
};

const char* GetTagName(RECORD_TAG tag);

}

