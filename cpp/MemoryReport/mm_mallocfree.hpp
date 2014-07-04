#pragma once

namespace mm {

void* Malloc(RECORD_TAG tag, size_t sz);
void Free(void* p);

}

