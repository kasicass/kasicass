#ifndef K1R_WINDOW_WINDOW_H
#define K1R_WINDOW_WINDOW_H

#include "K1RBase.h"
#include "K1RPropList.h"
#include "K1RSharedPtr.h"
#include "K1RController.h"
#include "K1RRenderer.h"

namespace K1R {

// base class, interface definition of window system
class Window
{
public:
	Window();
	virtual ~Window();

	void SetController(Controller *pCtrl);
	SharedPtr<Controller> GetController();

	void SetRenderer(Renderer *pRenderer);
	SharedPtr<Renderer> GetRenderer();

	virtual void Initialize() = 0;
	virtual void Run() = 0;
	virtual void Release() = 0;

private:
	Window(Window&);
	Window& operator= (const Window&);

private:
	SharedPtr<Controller> m_pController;
	SharedPtr<Renderer> m_pRenderer;
};


// global func
// ParamList = {
//   "winHeight" : 800,
//   "winWidth"  : 600,
//   "winTitle"  : "±êÌâÎÄ×Ö",
// }
Window *MakeWindow(PropList& ParamList);

};

#endif