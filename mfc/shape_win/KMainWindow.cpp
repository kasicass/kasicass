#include "KMainWindow.h"

BEGIN_MESSAGE_MAP(KMainWindow, CFrameWnd)
	ON_WM_NCHITTEST()
END_MESSAGE_MAP()

KMainWindow::KMainWindow()
{
	Create(NULL, _T("The Hello Application"), WS_OVERLAPPEDWINDOW, rectDefault,
		NULL, NULL, WS_EX_LAYERED);
	bgImage_.Load(L"background.png");

	BlitBackground();
}

void KMainWindow::BlitBackground()
{
	CPaintDC dc(this);
	UINT width  = bgImage_.GetWidth();
	UINT height = bgImage_.GetHeight();

	CDC memdc;
	memdc.CreateCompatibleDC(&dc);

	CBitmap bmp;
	bmp.CreateCompatibleBitmap(&dc, width, height);
	memdc.SelectObject(&bmp);

	Gdiplus::Graphics g(memdc);
	g.DrawImage(bgImage_, 0, 0, width, height);

	BLENDFUNCTION blend;
	ZeroMemory(&blend, sizeof(blend));
	blend.BlendOp = AC_SRC_OVER;
	blend.AlphaFormat = AC_SRC_ALPHA;
	blend.SourceConstantAlpha = 255;

	SIZE size;
	size.cx = width;
	size.cy = height;
	POINT src = {0, 0};

	BOOL r = UpdateLayeredWindow(&dc, NULL, &size, &memdc, &src, RGB(255, 255, 255), &blend, ULW_ALPHA);
	if (!r)
	{
		TCHAR buf[256];
		FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, GetLastError(), 0, buf, 256, NULL);
		MessageBox(buf, _T("warning"), MB_OK);
	}
}

LRESULT KMainWindow::OnNcHitTest(CPoint point)
{
	return HTCAPTION;
}

