#ifndef K_MAIN_WINDOW_H
#define K_MAIN_WINDOW_H

#include <afxwin.h>
#include "KGdiPlusBitmap.h"

class KMainWindow : public CFrameWnd
{
public:
	KMainWindow();

protected:
	afx_msg void OnPaint();
	DECLARE_MESSAGE_MAP();

private:
	KGdiPlusBitmap bgImage_;
};

#endif
