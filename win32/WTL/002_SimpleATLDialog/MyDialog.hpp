#pragma once

class MyDialog : public CDialogImpl<MyDialog>
{
public:
	enum { IDD = IDD_MYDIALOG };

	BEGIN_MSG_MAP(MyDialog)
		MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		MESSAGE_HANDLER(WM_CLOSE, OnClose)
		COMMAND_ID_HANDLER(IDOK, OnOKCancel)
		COMMAND_ID_HANDLER(IDCANCEL, OnOKCancel)
	END_MSG_MAP()

	LRESULT OnInitDialog(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		CenterWindow();
		return TRUE;
	}

	LRESULT OnClose(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		EndDialog(IDCANCEL);
		return 0;
	}

	LRESULT OnOKCancel(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL &bHandled)
	{
		EndDialog(wID);
		return 0;
	}
};

