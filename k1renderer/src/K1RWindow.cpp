#include "K1RWindow.h"

#if K1R_WIN32
#include "K1RWindowWin32.h"
#endif

namespace K1R {

//
// class Window
//

Window::Window()
 : m_pController(NULL)
{ }

Window::~Window()
{
	m_pController = NULL;
}

void Window::SetController(ControllerPtr pCtrl)
{
	m_pController = pCtrl;
}

ControllerPtr Window::GetController()
{
	return m_pController;
}

void Window::SetRenderer(RendererPtr pRenderer)
{
	m_pRenderer = pRenderer;
}

RendererPtr Window::GetRenderer()
{
	return m_pRenderer;
}



// global func
Window *MakeWindow(PropList& ParamList)
{
#if K1R_WIN32
	return new WindowWin32(ParamList);
#endif
}

}