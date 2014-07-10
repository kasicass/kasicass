#include "mm_report.hpp"
#include <atomic>
#include <sstream>
#include <iostream>

namespace mm {

#if defined(MEMORY_REPORT_ENABLE)

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

	void inccnt(RECORD_TAG tag, size_t cnt);
	void deccnt(RECORD_TAG tag, size_t cnt);

	std::string report();

private:
	ReportManager(const ReportManager& rhs);
	ReportManager& operator=(const ReportManager& rhs);

private:
	std::atomic_size_t tags_[TAG_COUNT];
	std::atomic_size_t cnts_[TAG_COUNT];
	std::atomic_size_t peekcnts_[TAG_COUNT];
};

ReportManager& ReportManager::instance()
{
	static ReportManager s_instance;
	return s_instance;
}

ReportManager::ReportManager()
{
	tags_[TAG_MM_SELF] = sizeof(*this);
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

void ReportManager::inccnt(RECORD_TAG tag, size_t cnt)
{
	cnts_[tag] += cnt;
	if (cnts_[tag] > peekcnts_[tag])
	{
		// thread-unsafe
		size_t v = cnts_[tag];
		peekcnts_[tag] = v;
	}
}

void ReportManager::deccnt(RECORD_TAG tag, size_t cnt)
{
	cnts_[tag] -= cnt;
}

std::string ReportManager::report()
{
	std::stringstream ss;
	size_t total = 0;
	ss << "[MM] **** MmSystem Memory Report ****" << std::endl;

	for (size_t i = 0; i < TAG_COUNT; ++i)
	{
		total += tags_[i];
		ss << "[MM] " << GetTagName((mm::RECORD_TAG)i) << ": "
		   << tags_[i] << " bytes(" << float(tags_[i]) / 1024.f / 1024.f << " Mb), "
		   << "(" << cnts_[i] << "/" << peekcnts_[i] << ") cnts"
		   << std::endl;
	}
	ss << "[MM] ---------------" << std::endl;
	ss << "[MM] total: " << total << " bytes(" << float(total) / 1024.f / 1024.f << " Mb)" << std::endl;

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

void RecordIncCount(RECORD_TAG tag, size_t cnt)
{
	ReportManager::instance().inccnt(tag, cnt);
}

void RecordDecCount(RECORD_TAG tag, size_t cnt)
{
	ReportManager::instance().deccnt(tag, cnt);
}

std::string MemReport()
{
	return ReportManager::instance().report();
}

#else

void RecordAlloc(RECORD_TAG tag, size_t sz)
{
}

void RecordDealloc(RECORD_TAG tag, size_t sz)
{
}

void RecordIncCount(RECORD_TAG tag, size_t cnt)
{
}

void RecordDecCount(RECORD_TAG tag, size_t cnt)
{
}

std::string MemReport()
{
	return std::string("MemoryReport not enabled");
}

#endif

}

