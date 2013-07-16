#include "KMainWindow.h"

BEGIN_MESSAGE_MAP(KMainWindow, CFrameWnd)
	ON_WM_PAINT()
END_MESSAGE_MAP()

KMainWindow::KMainWindow()
{
	Create(NULL, _T("The Hello Application"));
}

void KMainWindow::OnPaint()
{
	CPaintDC dc(this);

	CRect rect;
	GetClientRect(&rect);

	dc.DrawText("Hello, MFC", -1, &rect, DT_SINGLELINE | DT_CENTER | DT_VCENTER);
}

