#include "KMainWindow.h"
#include "resource.h"

BEGIN_MESSAGE_MAP(KMainWindow, CFrameWnd)
	ON_WM_PAINT()
END_MESSAGE_MAP()

KMainWindow::KMainWindow()
{
	Create(NULL, _T("The Hello Application"));
	btnTest_.Create(NULL, WS_CHILD|WS_VISIBLE|BS_OWNERDRAW, CRect(10,10,100,100), this, IDR_BUTTON_ID);
	btnTest_.LoadBitmaps(IDR_BUTTON_NORMAL, IDR_BUTTON_DOWN);
	btnTest_.SizeToContent();
}

