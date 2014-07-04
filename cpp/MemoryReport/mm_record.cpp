#include "mm_report.hpp"
#include <atomic>
#include <sstream>
#include <iostream>

namespace mm {

//
// ReportManager
//

class ReportManager
{
public:
	static ReportManager& instance();

public:
	ReportManager();
	~ReportManager();

	void alloc(RECORD_TAG tag, size_t sz);
	void dealloc(RECORD_TAG tag, size_t sz);

	std::string report();

private:
	ReportManager(const ReportManager& rhs);
	ReportManager& operator=(const ReportManager& rhs);

private:
	std::atomic_size_t tags_[TAG_COUNT];
};

ReportManager& ReportManager::instance()
{
	static ReportManager s_instance;
	return s_instance;
}

ReportManager::ReportManager()
{
}

ReportManager::~ReportManager()
{
}

void ReportManager::alloc(RECORD_TAG tag, size_t sz)
{
	tags_[tag] += sz;
}

void ReportManager::dealloc(RECORD_TAG tag, size_t sz)
{
	tags_[tag] -= sz;
}

std::string ReportManager::report()
{
	std::stringstream ss;
	size_t total = 0;
	for (size_t i = 0; i < TAG_COUNT; ++i)
	{
		total += tags_[i];
		ss << GetTagName((mm::RECORD_TAG)i) << ": " << tags_[i] << " bytes" << std::endl;
	}
	ss << "---------------" << std::endl;
	ss << "total: " << total << std::endl;
	return ss.str();
}


//
// mm::XXX() func
//

void RecordAlloc(RECORD_TAG tag, size_t sz)
{
	ReportManager::instance().alloc(tag, sz);
}

void RecordDealloc(RECORD_TAG tag, size_t sz)
{
	ReportManager::instance().dealloc(tag, sz);
}

std::string MemReport()
{
	return ReportManager::instance().report();
}

}

