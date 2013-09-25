#ifndef K_APP_H
#define K_APP_H

#include <afxwin.h>
#include <gdiplus.h>

class KMyApp : public CWinApp
{
public:
	virtual BOOL InitInstance();
	virtual int ExitInstance();

private:
	ULONG_PTR token_;
};

#endif
