#ifndef K_MAIN_WINDOW_H
#define K_MAIN_WINDOW_H

#include <afxwin.h>
#include "KGdiPlusBitmap.h"

class KMainWindow : public CFrameWnd
{
public:
	KMainWindow();

protected:
	DECLARE_MESSAGE_MAP();
	afx_msg LRESULT OnNcHitTest(CPoint point);

private:
	void BlitBackground();

private:
	KGdiPlusBitmap bgImage_;
};

#endif
