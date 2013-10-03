#pragma once

#include <windows.h>
#include <gdiplus.h>

namespace Ux {

class GdiPlusBitmap
{
public:
	GdiPlusBitmap();
	GdiPlusBitmap(LPCWSTR file);
	virtual ~GdiPlusBitmap();

	void release();

	bool load(LPCWSTR file);

	UINT width();
	UINT height();

	operator Gdiplus::Bitmap*() const { return bitmap_; }

private:
	Gdiplus::Bitmap *bitmap_;
};

class GdiPlusBitmapResource
{
public:
	GdiPlusBitmapResource();
	GdiPlusBitmapResource(LPCTSTR name, LPCTSTR type = RT_RCDATA, HMODULE hInst = NULL);
	GdiPlusBitmapResource(UINT id, LPCTSTR type = RT_RCDATA, HMODULE hInst = NULL);
	GdiPlusBitmapResource(UINT id, UINT type, HMODULE hInst = NULL);
	virtual ~GdiPlusBitmapResource();

	void release();

	bool load(LPCTSTR name, LPCTSTR type = RT_RCDATA, HMODULE hInst = NULL);
	bool load(UINT id, LPCTSTR type = RT_RCDATA, HMODULE hInst = NULL);

	UINT width();
	UINT height();

	operator Gdiplus::Bitmap*() const { return bitmap_; }

private:
	Gdiplus::Bitmap *bitmap_;
	HGLOBAL hBuffer_;
};

}