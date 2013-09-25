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

#endif
