#ifndef KGDIPLUS_BITMAP_H
#define KGDIPLUS_BITMAP_H

#include <windows.h>
#include <gdiplus.h>

class KGdiPlusBitmap
{
public:
	KGdiPlusBitmap();
	KGdiPlusBitmap(LPCWSTR file);
	virtual ~KGdiPlusBitmap();

	void Empty();
	bool Load(LPCWSTR file);

	UINT GetWidth();
	UINT GetHeight();

	operator Gdiplus::Bitmap*() const { return bitmap_; }

protected:
	Gdiplus::Bitmap *bitmap_;
};

class KGdiPlusBitmapResource : public KGdiPlusBitmap
{
public:
	KGdiPlusBitmapResource();
	KGdiPlusBitmapResource(LPCTSTR name, LPCTSTR type = RT_RCDATA, HMODULE hInst = NULL);
	KGdiPlusBitmapResource(UINT id, LPCTSTR type = RT_RCDATA, HMODULE hInst = NULL);
	KGdiPlusBitmapResource(UINT id, UINT type, HMODULE hInst = NULL);
	virtual ~KGdiPlusBitmapResource();

	void Empty();

	bool Load(LPCTSTR name, LPCTSTR type = RT_RCDATA, HMODULE hInst = NULL);
	bool Load(UINT id, LPCTSTR type = RT_RCDATA, HMODULE hInst = NULL);
	bool Load(UINT id, UINT type, HMODULE hInst = NULL);

protected:
	HGLOBAL hBuffer_;
};

#endif
