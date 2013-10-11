#include <Windows.Devices.Enumeration.h>
#include <wrl/event.h>
#include <stdio.h>

using namespace ABI::Windows::Devices::Enumeration;
using namespace ABI::Windows::Foundation;
using namespace Microsoft::WRL;
using namespace Microsoft::WRL::Wrappers;

int PrintError(unsigned int line, HRESULT hr)
{
	wprintf_s(L"ERROR: Line:%d HRESULT:0x%X\n", line, hr);
	return hr;
}

int wmain()
{
	typedef __FITypedEventHandler_2_Windows__CDevices__CEnumeration__CDeviceWatcher_Windows__CDevices__CEnumeration__CDeviceInformation AddedHandler;
	typedef __FITypedEventHandler_2_Windows__CDevices__CEnumeration__CDeviceWatcher_IInspectable EnumerationCompletedHandler;
	typedef __FITypedEventHandler_2_Windows__CDevices__CEnumeration__CDeviceWatcher_IInspectable StoppedHandler;

	unsigned int deviceCount = 0;

	EventRegistrationToken addedToken;
	EventRegistrationToken stoppedToken;
	EventRegistrationToken enumCompletedToken;

	RoInitializeWrapper initialze(RO_INIT_MULTITHREADED);
	if (FAILED(initialze))
	{
		return PrintError(__LINE__, initialze);
	}

	Event enumerationCompleted(CreateEventEx(nullptr, nullptr, CREATE_EVENT_MANUAL_RESET, WRITE_OWNER | EVENT_ALL_ACCESS));
	HRESULT hr = enumerationCompleted.IsValid() ? S_OK : HRESULT_FROM_WIN32(GetLastError());
	if (FAILED(hr))
	{
		return PrintError(__LINE__, initialze);
	}

	ComPtr<IDeviceInformationStatics> watcherFactory;
	hr = GetActivationFactory(HStringReference(RuntimeClass_Windows_Devices_Enumeration_DeviceInformation).Get(), &watcherFactory);
	if (FAILED(hr))
	{
		return PrintError(__LINE__, initialze);
	}

	ComPtr<IDeviceWatcher> watcher;
	hr = watcherFactory->CreateWatcher(&watcher);
	if (FAILED(hr))
	{
		return PrintError(__LINE__, initialze);
	}

	hr = watcher->add_Added(Callback<AddedHandler>([&deviceCount](IDeviceWatcher* watcher, IDeviceInformation*) -> HRESULT
	{
		wprintf_s(L"Added device...\n");
		deviceCount++;
		if (deviceCount == 10)
		{
			return watcher->Stop();
		}
		return S_OK;
	}).Get(), &addedToken);
	if (FAILED(hr))
	{
		return PrintError(__LINE__, initialze);
	}

	hr = watcher->add_Stopped(Callback<StoppedHandler>([=, &enumerationCompleted](IDeviceWatcher* watcher, IInspectable*) -> HRESULT
	{
		wprintf_s(L"Device enumeration stopped.\nRemoving event handles...");

		HRESULT hr1 = watcher->remove_Added(addedToken);
		HRESULT hr2 = watcher->remove_Stopped(stoppedToken);
		HRESULT hr3 = watcher->remove_EnumerationCompleted(enumCompletedToken);

		SetEvent(enumerationCompleted.Get());
		return FAILED(hr1) ? hr1 : FAILED(hr2) ? hr2 : hr3;
	}).Get(), &stoppedToken);
	if (FAILED(hr))
	{
		return PrintError(__LINE__, initialze);
	}

	hr = watcher->add_EnumerationCompleted(Callback<EnumerationCompletedHandler>([](IDeviceWatcher* watcher, IInspectable*) -> HRESULT
	{
		wprintf_s(L"Enumeration completed.\n");
		return watcher->Stop();
	}).Get(), &enumCompletedToken);
	if (FAILED(hr))
	{
		return PrintError(__LINE__, initialze);
	}

	wprintf_s(L"Starting device enumeration...\n");
	hr = watcher->Start();
	if (FAILED(hr))
	{
		return PrintError(__LINE__, initialze);
	}

	WaitForSingleObjectEx(enumerationCompleted.Get(), INFINITE, FALSE);
	wprintf_s(L"Enumerated %u devices.\n", deviceCount);
	return 0;
}

