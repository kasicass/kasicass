#pragma once

#include <Windows.h>

namespace Ux {

class WndDC
{
public:
	WndDC(HWND hWnd);
	~WndDC();

	operator HDC() const { return hDC_; }

private:
	HWND hWnd_;
	HDC hDC_;
};

class MemoryDC
{
public:
	MemoryDC();
	MemoryDC(HDC hdc, int width, int height);
	~MemoryDC();

	void createCompatibleDC(HDC hdc, int width, int height);

	operator HDC() const { return hMemDC_; }

private:
	void release();

private:
	HBITMAP hBitmap_;
	HDC hMemDC_;
};

}
