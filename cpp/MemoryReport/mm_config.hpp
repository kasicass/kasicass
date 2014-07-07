#pragma once

namespace mm {

enum RECORD_TAG {
	TAG_MISC = 0,
	TAG_FOO,
	TAG_BAR,
	TAG_VECTOR_INT,
	TAG_LIST_INT,
	TAG_MAP,
	TAG_BUFFER,

	TAG_COUNT,
};

const char* GetTagName(RECORD_TAG tag);

}

