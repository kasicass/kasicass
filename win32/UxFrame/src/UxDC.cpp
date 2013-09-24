#include "UxDC.hpp"

namespace Ux {

//
// WndDC
//

WndDC::WndDC(HWND hWnd) : hWnd_(hWnd)
{
	hDC_ = ::GetDC(hWnd_);
}

WndDC::~WndDC()
{
	::ReleaseDC(hWnd_, hDC_);
}


//
// MemoryDC
//

MemoryDC::MemoryDC() : hBitmap_(0), hMemDC_(0)
{
}

MemoryDC::MemoryDC(HDC hdc, int width, int height) : hBitmap_(0), hMemDC_(0)
{
	createCompatibleDC(hdc, width, height);
}

MemoryDC::~MemoryDC()
{
	release();
}

void MemoryDC::release()
{
	if (hBitmap_)
	{
		::DeleteObject(hBitmap_);
		hBitmap_ = 0;
	}

	if (hMemDC_)
	{
		::DeleteDC(hMemDC_);
		hMemDC_ = 0;
	}
}

void MemoryDC::createCompatibleDC(HDC hdc, int width, int height)
{
	release();

	hMemDC_  = CreateCompatibleDC(hdc);
	hBitmap_ = CreateCompatibleBitmap(hdc, width, height);
	::SelectObject(hMemDC_, hBitmap_);
}

}
