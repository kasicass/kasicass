#pragma once

#include <string>

namespace mm {

void RecordAlloc(size_t sz);
void RecordDealloc(size_t sz);

std::string MemReport();

}

