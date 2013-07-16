#include "KTransparentButton.h"

IMPLEMENT_DYNAMIC(KTransparentButton, CButton)

KTransparentButton::KTransparentButton()
{}

BOOL KTransparentButton::LoadBitmaps(LPCTSTR lpszBitmapResource,
				LPCTSTR lpszBitmapResourceSel,
				LPCTSTR lpszBitmapResourceFocus,
				LPCTSTR lpszBitmapResourceDisabled)
{
	m_bitmap.DeleteObject();
	m_bitmapSel.DeleteObject();
	m_bitmapFocus.DeleteObject();
	m_bitmapDisabled.DeleteObject();

	if (!m_bitmap.LoadBitmap(lpszBitmapResource))
	{
		TRACE(traceAppMsg, 0, "Failed to load bitmap for normal image.\n");
		return FALSE;
	}

	BOOL bAllLoaded = TRUE;
	if (lpszBitmapResourceSel != NULL)
	{
		if (!m_bitmapSel.LoadBitmap(lpszBitmapResourceSel))
		{
			TRACE(traceAppMsg, 0, "Failed to load bitmap for selected image.\n");
			bAllLoaded = FALSE;
		}
	}

	if (lpszBitmapResourceFocus != NULL)
	{
		if (!m_bitmapFocus.LoadBitmap(lpszBitmapResourceFocus))
		{
			TRACE(traceAppMsg, 0, "Failed to load bitmap for focus image.\n");
			bAllLoaded = FALSE;
		}
	}

	if (lpszBitmapResourceDisabled != NULL)
	{
		if (!m_bitmapDisabled.LoadBitmap(lpszBitmapResourceDisabled))
		{
			TRACE(traceAppMsg, 0, "Failed to load bitmap for disabled image.\n");
			bAllLoaded = FALSE;
		}
	}

	return bAllLoaded;
}

BOOL KTransparentButton::LoadBitmaps(UINT nIDBitmapResource,
					UINT nIDBitmapResourceSel,
					UINT nIDBitmapResourceFocus,
					UINT nIDBitmapResourceDisabled)
{
	return LoadBitmaps(MAKEINTRESOURCE(nIDBitmapResource),
		MAKEINTRESOURCE(nIDBitmapResourceSel),
		MAKEINTRESOURCE(nIDBitmapResourceFocus),
		MAKEINTRESOURCE(nIDBitmapResourceDisabled));
}

void KTransparentButton::SizeToContent()
{
	ASSERT(m_bitmap.m_hObject != NULL);
	CSize bitmapSize;
	BITMAP bmInfo;
	m_bitmap.GetObject(sizeof(bmInfo), &bmInfo);
	SetWindowPos(NULL, -1, -1, bmInfo.bmWidth, bmInfo.bmHeight,
		SWP_NOMOVE|SWP_NOZORDER|SWP_NOREDRAW|SWP_NOACTIVATE);
}

void KTransparentButton::DrawItem(LPDRAWITEMSTRUCT lpDIS)
{
	ASSERT(lpDIS != NULL);
	ASSERT(m_bitmap.m_hObject != NULL);

	// use the main bitmap for up, the selected bitmap for down
	CBitmap *pBitmap = &m_bitmap;
	UINT state = lpDIS->itemState;
	if ((state & ODS_SELECTED) && m_bitmapSel.m_hObject != NULL)
		pBitmap = &m_bitmapSel;
	if ((state & ODS_FOCUS) && m_bitmapFocus.m_hObject != NULL)
		pBitmap = &m_bitmapFocus;
	if ((state & ODS_DISABLED) && m_bitmapDisabled.m_hObject != NULL)
		pBitmap = &m_bitmapDisabled;

	// draw the whole button
	CDC *pDC = CDC::FromHandle(lpDIS->hDC);
	CDC memDC;
	memDC.CreateCompatibleDC(pDC);
	CBitmap *pOld = memDC.SelectObject(pBitmap);
	if (pOld == NULL)
		return;

	CRect rect;
	rect.CopyRect(&lpDIS->rcItem);
	pDC->TransparentBlt(rect.left, rect.top, rect.Width(), rect.Height(),
		&memDC, 0, 0, rect.Width(), rect.Height(), RGB(255, 0, 255));
}

