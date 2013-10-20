// WTL looks for icon, menu, accelerator, title using IDR_MAINFRAME.

#pragma once

class MyWindow : public CFrameWindowImpl<MyWindow>
{
public:
	DECLARE_FRAME_WND_CLASS(_T("First ATL Window"), IDR_MAINFRAME);

	BEGIN_MSG_MAP(MyWindow)
		MSG_WM_CREATE(OnCreate)
		MSG_WM_DESTROY(OnDestroy)
		MSG_WM_TIMER(OnTimer)
		MSG_WM_ERASEBKGND(OnEraseBkgnd)
		CHAIN_MSG_MAP(CFrameWindowImpl<MyWindow>)
	END_MSG_MAP()

	LRESULT OnCreate(LPCREATESTRUCT lpcs)
	{
		SetTimer(1, 1000);
		SetMsgHandled(false);
		return 0;
	}

	void OnDestroy()
	{
		KillTimer(1);
		SetMsgHandled(false);
	}

	void OnTimer(UINT_PTR uTimerID)
	{
		if (1 != uTimerID)
			SetMsgHandled(false);
		else
			RedrawWindow();
	}

	LRESULT OnEraseBkgnd(HDC hdc)
	{
		CDCHandle dc(hdc);
		CRect rc;
		SYSTEMTIME st;
		CString sTime;

		GetClientRect(rc);
		GetLocalTime(&st);
		sTime.Format(_T("The time is %d:%02d:%02d"), st.wHour, st.wMinute, st.wSecond);

		dc.SaveDC();
		dc.SetBkColor(RGB(255,153,0));
		dc.SetTextColor(RGB(0,0,0));
		dc.ExtTextOut(0, 0, ETO_OPAQUE, rc, sTime, sTime.GetLength(), NULL);
		dc.RestoreDC(-1);

		return 1;  // we erased the backgroudn (ExtTextOut did it)
	}
};

