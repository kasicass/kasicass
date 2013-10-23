// WTL looks for icon, menu, accelerator, title using IDR_MAINFRAME.

#pragma once

class MyWindow : public CFrameWindowImpl<MyWindow>
{
public:
	DECLARE_FRAME_WND_CLASS(_T("First ATL Window"), IDR_MAINFRAME);
	BEGIN_MSG_MAP(MyWindow)
		CHAIN_MSG_MAP(CFrameWindowImpl<MyWindow>)
	END_MSG_MAP()
};

