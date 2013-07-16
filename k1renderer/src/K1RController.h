#ifndef K1R_WINDOW_CONTROLLER_H
#define K1R_WINDOW_CONTROLLER_H

#include <memory>

namespace K1R {

enum KeyPress
{
	KEY_NONE,
	KEY_A,
	KEY_B,
	KEY_C,
	KEY_D,
	KEY_E,
	KEY_F,
	KEY_G,
	KEY_H,
	KEY_I,
	KEY_J,
	KEY_K,
	KEY_L,
	KEY_M,
	KEY_N,
	KEY_O,
	KEY_P,
	KEY_Q,
	KEY_R,
	KEY_S,
	KEY_T,
	KEY_U,
	KEY_V,
	KEY_W,
	KEY_X,
	KEY_Y,
	KEY_Z,
};

class KeyState
{
public:
	KeyState() {}
	virtual ~KeyState() {}

	virtual bool IsCtrl() const = 0;
	virtual bool IsShift() const = 0;
	virtual bool IsLButton() const = 0;
	virtual bool IsMButton() const = 0;
	virtual bool IsRButton() const = 0;
};

class Controller
{
public:
	Controller() {}
	virtual ~Controller() {}

	virtual void OnDestroy() {}     // when windows destroy

	virtual void OnKeyDown(KeyPress key) {}
	virtual void OnLButtonDown(int x, int y, KeyState& kState) {}
	virtual void OnMouseMove(int x, int y, KeyState& kState) {}
};
typedef std::shared_ptr<Controller> ControllerPtr;

}

#endif