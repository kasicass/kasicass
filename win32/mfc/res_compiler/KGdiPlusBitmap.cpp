#include "KGdiPlusBitmap.h"
#include <assert.h>

// ------------------------------------------------
// KGdiPlusBitmap
// ------------------------------------------------

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


// ------------------------------------------------
// KGdiPlusBitmapResource
// ------------------------------------------------

KGdiPlusBitmapResource::KGdiPlusBitmapResource()
	: hBuffer_(NULL)
{}

KGdiPlusBitmapResource::KGdiPlusBitmapResource(LPCTSTR name, LPCTSTR type, HMODULE hInst)
{
	hBuffer_ = NULL;
	Load(name, type, hInst);
}

KGdiPlusBitmapResource::KGdiPlusBitmapResource(UINT id, LPCTSTR type, HMODULE hInst)
{
	hBuffer_ = NULL;
	Load(id, type, hInst);
}

KGdiPlusBitmapResource::KGdiPlusBitmapResource(UINT id, UINT type, HMODULE hInst)
{
	hBuffer_ = NULL;
	Load(id, type, hInst);
}

KGdiPlusBitmapResource::~KGdiPlusBitmapResource()
{
	Empty();
}

void KGdiPlusBitmapResource::Empty()
{
	KGdiPlusBitmap::Empty();
	if (hBuffer_)
	{
		::GlobalUnlock(hBuffer_);
		::GlobalFree(hBuffer_);
		hBuffer_ = NULL;
	}
}

bool KGdiPlusBitmapResource::Load(LPCTSTR name, LPCSTR type, HMODULE hInst)
{
	Empty();

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

bool KGdiPlusBitmapResource::Load(UINT id, LPCTSTR type, HMODULE hInst)
{
	return Load(MAKEINTRESOURCE(id), type, hInst);
}

bool KGdiPlusBitmapResource::Load(UINT id, UINT type, HMODULE hInst)
{
	return Load(MAKEINTRESOURCE(id), MAKEINTRESOURCE(type), hInst);
}

