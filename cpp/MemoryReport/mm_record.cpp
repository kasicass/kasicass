#include "mm_report.hpp"
#include <atomic>
#include <sstream>

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

	void alloc(size_t sz);
	void dealloc(size_t sz);

	std::string report();

private:
	ReportManager(const ReportManager& rhs);
	ReportManager& operator=(const ReportManager& rhs);

private:
	std::atomic_size_t total_;
};

ReportManager& ReportManager::instance()
{
	static ReportManager s_instance;
	return s_instance;
}

ReportManager::ReportManager() : total_(0)
{
}

ReportManager::~ReportManager()
{
}

void ReportManager::alloc(size_t sz)
{
	total_ += sz;
}

void ReportManager::dealloc(size_t sz)
{
	total_ -= sz;
}

std::string ReportManager::report()
{
	std::stringstream ss;
	ss << "Total: " << total_ << " bytes";
	return ss.str();
}


//
// mm::XXX() func
//

void RecordAlloc(size_t sz)
{
	ReportManager::instance().alloc(sz);
}

void RecordDealloc(size_t sz)
{
	ReportManager::instance().dealloc(sz);
}

std::string MemReport()
{
	return ReportManager::instance().report();
}

}

