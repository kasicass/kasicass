#include <Windows.Foundation.h>
#include <Windows.System.Threading.h>
#include <wrl/event.h>
#include <stdio.h>
#include <objbase.h>

using namespace ABI::Windows::Foundation;
using namespace ABI::Windows::System::Threading;
using namespace Microsoft::WRL;
using namespace Microsoft::WRL::Wrappers;

int PrintError(unsigned long line, HRESULT hr)
{
	wprintf_s(L"ERROR: Line:%d HRESULT:0x%X\n", line, hr);
	return hr;
}

bool IsPrime(int n)
{
	if (n < 2)
	{
		return false;
	}
	for (int i = 2; i < n; ++i)
	{
		if ((n % i) == 0)
		{
			return false;
		}
	}
	return true;
}

int wmain()
{
	RoInitializeWrapper initialize(RO_INIT_MULTITHREADED);
	if (FAILED(initialize))
	{
		return PrintError(__LINE__, initialize);
	}

	ComPtr<IThreadPoolStatics> threadPool;
	HRESULT hr = GetActivationFactory(HStringReference(RuntimeClass_Windows_System_Threading_ThreadPool).Get(), &threadPool);
	if (FAILED(hr))
	{
		return PrintError(__LINE__, hr);
	}

	Event threadCompleted(CreateEventEx(nullptr, nullptr, CREATE_EVENT_MANUAL_RESET, WRITE_OWNER | EVENT_ALL_ACCESS));
	hr = threadCompleted.IsValid() ? S_OK : HRESULT_FROM_WIN32(GetLastError());
	if (FAILED(hr))
	{
		return PrintError(__LINE__, hr);
	}

	wprintf_s(L"Starting thread...\n");

	ComPtr<IAsyncAction> asyncAction;
	hr = threadPool->RunAsync(Callback<IWorkItemHandler>([&threadCompleted](IAsyncAction* asyncAction) -> HRESULT
	{
		const unsigned int start = 0;
		const unsigned int end = 100000;
		unsigned int primeCount = 0;
		for (int n = start; n < end; n++)
		{
			if (IsPrime(n))
			{
				primeCount++;
			}
		}

		wprintf_s(L"There are %u prime numbers from %u to %u.\n", primeCount, start, end);

		SetEvent(threadCompleted.Get());
		return S_OK;
	}).Get(), &asyncAction);
	if (FAILED(hr))
	{
		return PrintError(__LINE__, hr);
	}

	wprintf_s(L"Waiting for thread...\n");
	WaitForSingleObjectEx(threadCompleted.Get(), INFINITE, FALSE);
	wprintf_s(L"Finished.\n");
	return 0;
}

