#ifndef K_MAIN_WINDOW_H
#define K_MAIN_WINDOW_H

#include <afxwin.h>
#include <afxext.h>

class KMainWindow : public CFrameWnd
{
public:
	KMainWindow();

protected:
	DECLARE_MESSAGE_MAP();

private:
	CBitmapButton btnTest_;
};

#endif
