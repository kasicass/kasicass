#include "KGdiPlusBitmap.h"
#include <assert.h>

KGdiPlusBitmap::KGdiPlusBitmap()
	: bitmap_(NULL)
{}

KGdiPlusBitmap::KGdiPlusBitmap(LPCWSTR file)
	: bitmap_(NULL)
{
	Load(file);
}

KGdiPlusBitmap::~KGdiPlusBitmap()
{
	Empty();
}

void KGdiPlusBitmap::Empty()
{
	delete bitmap_;
	bitmap_ = NULL;
}

bool KGdiPlusBitmap::Load(LPCWSTR file)
{
	Empty();
	bitmap_ = Gdiplus::Bitmap::FromFile(file);
	return bitmap_->GetLastStatus() == Gdiplus::Ok;
}

UINT KGdiPlusBitmap::GetWidth()
{
	assert(bitmap_ != NULL);
	return bitmap_->GetWidth();
}

UINT KGdiPlusBitmap::GetHeight()
{
	assert(bitmap_ != NULL);
	return bitmap_->GetHeight();
}

