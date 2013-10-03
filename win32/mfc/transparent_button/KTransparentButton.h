#ifndef K_TRANSPARENT_BUTTON_H
#define K_TRANSPARENT_BUTTON_H

#include <afxwin.h>

class KTransparentButton : public CButton
{
	DECLARE_DYNAMIC(KTransparentButton)
public:
	// Construction
	KTransparentButton();

	BOOL LoadBitmaps(LPCTSTR lpszBitmapResource,
					LPCTSTR lpszBitmapResourceSel = NULL,
					LPCTSTR lpszBitmapResourceFocus = NULL,
					LPCTSTR lpszBitmapResourceDisabled = NULL);
	BOOL LoadBitmaps(UINT nIDBitmapResource,
					UINT nIDBitmapResourceSel = 0,
					UINT nIDBitmapResourceFocus = 0,
					UINT nIDBitmapResourceDisabled = 0);

	// Operations
	void SizeToContent();

protected:
	CBitmap m_bitmap;
	CBitmap m_bitmapSel;
	CBitmap m_bitmapFocus;
	CBitmap m_bitmapDisabled;

	virtual void DrawItem(LPDRAWITEMSTRUCT lpDIS);
};

#endif
