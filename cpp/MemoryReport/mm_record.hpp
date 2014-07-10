#pragma once

#include <string>

namespace mm {

void RecordAlloc(RECORD_TAG tag, size_t sz);
void RecordDealloc(RECORD_TAG tag, size_t sz);

void RecordIncCount(RECORD_TAG tag, size_t cnt);
void RecordDecCount(RECORD_TAG tag, size_t cnt);

std::string MemReport();

}

