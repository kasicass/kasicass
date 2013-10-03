#include "KMainWindow.h"

BEGIN_MESSAGE_MAP(KMainWindow, CFrameWnd)
	ON_WM_PAINT()
END_MESSAGE_MAP()

KMainWindow::KMainWindow()
{
	Create(NULL, _T("The Hello Application"));
	bgImage_.Load(L"blacksmith.bmp");
}

void KMainWindow::OnPaint()
{
	CPaintDC dc(this);

	Gdiplus::Graphics g(dc);
	g.DrawImage(bgImage_, 0, 0, bgImage_.GetWidth(), bgImage_.GetHeight());

	CRect rect;
	GetClientRect(&rect);

	dc.DrawText(_T("Hello, MFC"), -1, &rect, DT_SINGLELINE | DT_CENTER | DT_VCENTER);
}

