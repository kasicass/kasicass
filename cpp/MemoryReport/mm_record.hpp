#pragma once

#include <string>

namespace mm {

void RecordAlloc(RECORD_TAG tag, size_t sz);
void RecordDealloc(RECORD_TAG tag, size_t sz);

std::string MemReport();

}

