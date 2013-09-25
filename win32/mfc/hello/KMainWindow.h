#ifndef K_MAIN_WINDOW_H
#define K_MAIN_WINDOW_H

#include <afxwin.h>

class KMainWindow : public CFrameWnd
{
public:
	KMainWindow();

protected:
	DECLARE_MESSAGE_MAP();
	afx_msg void OnPaint();
};

#endif
