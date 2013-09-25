#ifndef K_MAIN_WINDOW_H
#define K_MAIN_WINDOW_H

#include <afxwin.h>
#include "KTransparentButton.h"

class KMainWindow : public CFrameWnd
{
public:
	KMainWindow();

protected:
	DECLARE_MESSAGE_MAP();

private:
	KTransparentButton btnTest_;
};

#endif
