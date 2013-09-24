#include "UxGdiPlusBitmap.hpp"
#include <assert.h>

namespace Ux {

//
// GdiPlusBitmap
//

GdiPlusBitmap::GdiPlusBitmap()
	: bitmap_(NULL)
{}

GdiPlusBitmap::GdiPlusBitmap(LPCWSTR file)
	: bitmap_(NULL)
{
	load(file);
}

GdiPlusBitmap::~GdiPlusBitmap()
{
	release();
}

UINT GdiPlusBitmap::width()
{
	assert(bitmap_ != NULL);
	return bitmap_->GetWidth();
}

UINT GdiPlusBitmap::height()
{
	assert(bitmap_ != NULL);
	return bitmap_->GetHeight();
}

void GdiPlusBitmap::release()
{
	delete bitmap_;
	bitmap_ = NULL;
}

bool GdiPlusBitmap::load(LPCWSTR file)
{
	release();
	bitmap_ = Gdiplus::Bitmap::FromFile(file);
	if (!bitmap_)
		return false;

	if (bitmap_->GetLastStatus() != Gdiplus::Ok)
	{
		release();
		return false;
	}

	return true;
}


//
// KGdiPlusBitmapResource
//

GdiPlusBitmapResource::GdiPlusBitmapResource()
	: bitmap_(NULL), hBuffer_(NULL)
{}

GdiPlusBitmapResource::GdiPlusBitmapResource(LPCTSTR name, LPCTSTR type, HMODULE hInst)
{
	bitmap_  = NULL;
	hBuffer_ = NULL;
	load(name, type, hInst);
}

GdiPlusBitmapResource::GdiPlusBitmapResource(UINT id, LPCTSTR type, HMODULE hInst)
{
	bitmap_  = NULL;
	hBuffer_ = NULL;
	load(id, type, hInst);
}

GdiPlusBitmapResource::~GdiPlusBitmapResource()
{
	release();
}

UINT GdiPlusBitmapResource::width()
{
	assert(bitmap_ != NULL);
	return bitmap_->GetWidth();
}

UINT GdiPlusBitmapResource::height()
{
	assert(bitmap_ != NULL);
	return bitmap_->GetHeight();
}

void GdiPlusBitmapResource::release()
{
	delete bitmap_;
	bitmap_ = NULL;

	if (hBuffer_)
	{
		::GlobalUnlock(hBuffer_);
		::GlobalFree(hBuffer_);
		hBuffer_ = NULL;
	}
}

bool GdiPlusBitmapResource::load(LPCTSTR name, LPCSTR type, HMODULE hInst)
{
	release();

	HRSRC hRes = ::FindResource(hInst, name, type);
	if (!hRes)
		return false;

	DWORD imageSize = ::SizeofResource(hInst, hRes);
	if (!imageSize)
		return false;

	const void *pResData = ::LockResource(::LoadResource(hInst, hRes));
	if (!pResData)
		return false;

	hBuffer_ = ::GlobalAlloc(GMEM_MOVEABLE, imageSize);
	if (hBuffer_)
	{
		void *pBuffer = ::GlobalLock(hBuffer_);
		if (pBuffer)
		{
			CopyMemory(pBuffer, pResData, imageSize);

			IStream *pStream = NULL;
			if (::CreateStreamOnHGlobal(hBuffer_, FALSE, &pStream) == S_OK)
			{
				bitmap_ = Gdiplus::Bitmap::FromStream(pStream);
				pStream->Release();
				if (bitmap_)
				{
					if (bitmap_->GetLastStatus() == Gdiplus::Ok)
						return true;

					delete bitmap_;
					bitmap_ = NULL;	
				}
			}
			::GlobalUnlock(hBuffer_);
		}
		::GlobalFree(hBuffer_);
		hBuffer_ = NULL;
	}

	return false;
}

bool GdiPlusBitmapResource::load(UINT id, LPCTSTR type, HMODULE hInst)
{
	return load(MAKEINTRESOURCE(id), type, hInst);
}

}
