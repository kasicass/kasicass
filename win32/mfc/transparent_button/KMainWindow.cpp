#include "KMainWindow.h"
#include "resource.h"

BEGIN_MESSAGE_MAP(KMainWindow, CFrameWnd)
END_MESSAGE_MAP()

KMainWindow::KMainWindow()
{
	Create(NULL, _T("The Hello Application"));

	btnTest_.Create(NULL, WS_CHILD|WS_VISIBLE|BS_OWNERDRAW, CRect(10,10,100,100), this, 1);
	btnTest_.LoadBitmaps(IDR_IMAGE_NORMAL, IDR_IMAGE_DOWN);
	btnTest_.SizeToContent();
}

