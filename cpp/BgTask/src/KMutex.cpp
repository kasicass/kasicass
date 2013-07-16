#include "KMutex.h"

KMutex::KMutex()
{
	::InitializeCriticalSection(&cs_);
}

KMutex::~KMutex()
{
	::DeleteCriticalSection(&cs_);
}

void KMutex::lock()
{
	::EnterCriticalSection(&cs_);
}

void KMutex::unlock()
{
	::LeaveCriticalSection(&cs_);
}
