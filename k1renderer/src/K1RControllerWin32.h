#ifndef K1R_WINDOW_CONTROLLER_WIN32_H
#define K1R_WINDOW_CONTROLLER_WIN32_H

#include "K1RWin32Platform.h"
#include "K1RController.h"

namespace K1R {

class KeyPressWin32
{
public:
	KeyPressWin32(WPARAM wParam) : m_wParam(wParam) {}
	~KeyPressWin32() {}

	KeyPress GetKey();

private:
	WPARAM m_wParam;
};

class KeyStateWin32 : public KeyState
{
public:
	KeyStateWin32(WPARAM wParam) : m_wParam(wParam) {}
	virtual ~KeyStateWin32() {}

	virtual bool IsCtrl() const    { return (m_wParam & MK_CONTROL) != 0; }
	virtual bool IsShift() const   { return (m_wParam & MK_SHIFT) != 0; }
	virtual bool IsLButton() const { return (m_wParam & MK_LBUTTON) != 0; }
	virtual bool IsMButton() const { return (m_wParam & MK_MBUTTON) != 0; }
	virtual bool IsRButton() const { return (m_wParam & MK_RBUTTON) != 0; }

private:
	WPARAM m_wParam;
};

}

#endif