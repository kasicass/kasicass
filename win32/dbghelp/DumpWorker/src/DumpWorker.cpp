#include "DumpWorker.hpp"

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <DbgHelp.h>
#include <string>

typedef BOOL (WINAPI *MINIDUMPWRITEDUMP)(HANDLE hProcess, DWORD dwPid, HANDLE hFile, MINIDUMP_TYPE DumpType,
		CONST PMINIDUMP_EXCEPTION_INFORMATION ExceptionParam,
		CONST PMINIDUMP_USER_STREAM_INFORMATION UserStreamParam,
		CONST PMINIDUMP_CALLBACK_INFORMATION CallbackParam
		);

static std::string s_dumpFilename("test.dmp");
static DumpWorker::DUMP_TYPE s_dumpType = DumpWorker::MINI_DUMP;

static LONG WINAPI MyExceptionFilter(struct _EXCEPTION_POINTERS *ExceptionInfo)
{
	// get MiniDumpWriteDump() function pointer
	HMODULE hDll = NULL;
	hDll = LoadLibrary("dbghelp.dll");
	if (hDll == NULL)
	{
		char temp[2048];
		::GetCurrentDirectory(sizeof(temp), temp);

		std::string dbgpath = temp;
		dbgpath += "\\dbghelp.dll";

		hDll = LoadLibrary( dbgpath.c_str() );
		if (hDll == NULL)
			return EXCEPTION_CONTINUE_SEARCH;
	}

	MINIDUMPWRITEDUMP DumpFn = (MINIDUMPWRITEDUMP)::GetProcAddress( hDll, "MiniDumpWriteDump" );
	if(DumpFn == NULL)
		return EXCEPTION_CONTINUE_SEARCH;

	// dump it
	HANDLE hDumpFile = CreateFile(s_dumpFilename.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL ,NULL);
 
	MINIDUMP_EXCEPTION_INFORMATION loExceptionInfo;
	loExceptionInfo.ExceptionPointers = ExceptionInfo;
	loExceptionInfo.ThreadId = GetCurrentThreadId();
	loExceptionInfo.ClientPointers = TRUE;

	MINIDUMP_TYPE dtype = s_dumpType == DumpWorker::FULL_DUMP ? MiniDumpWithFullMemory : MiniDumpNormal;
	DumpFn(GetCurrentProcess(), GetCurrentProcessId(), hDumpFile, dtype, &loExceptionInfo, NULL, NULL);

	CloseHandle(hDumpFile);
	return EXCEPTION_EXECUTE_HANDLER;
}

static void MyMakeCrash()
{
	int *a = 0;
	*a = 1;
}

static void __cdecl MyInvalidParameterHandler(const wchar_t *, const wchar_t *, const wchar_t *, unsigned int, uintptr_t)
{
	MyMakeCrash();
}


//
// DumpWorker
//
void DumpWorker::Init(const char *dumpFilename, DumpWorker::DUMP_TYPE dtype)
{
	s_dumpFilename = dumpFilename;
	s_dumpType = dtype;

	SetUnhandledExceptionFilter(MyExceptionFilter);
	_set_invalid_parameter_handler(&MyInvalidParameterHandler);
	_set_purecall_handler(&MyMakeCrash);
	std::set_new_handler(MyMakeCrash);

	//_CrtSetReportMode(_CRT_ASSERT, 0);
}

