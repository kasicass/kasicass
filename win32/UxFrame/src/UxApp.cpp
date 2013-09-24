#include "UxApp.hpp"
#include <assert.h>

namespace Ux {

App* App::s_app = nullptr;

App& App::instance()
{
	assert(s_app);
	return *s_app;
}

App::App() : hInstance_(0)
{
	assert(s_app == nullptr);  // only one Ux::App instance.
	s_app = this;
}

App::~App()
{
}

void App::run()
{
	MSG msg;
	ZeroMemory(&msg, sizeof(msg));
	while (::GetMessage(&msg, NULL, 0, 0))
	{
		::TranslateMessage(&msg);
		::DispatchMessage(&msg);
	}
}

void App::setHINSTANCE(HINSTANCE hInstance)
{
	hInstance_ = hInstance;
}

HINSTANCE App::getHINSTANCE()
{
	return hInstance_;
}

}

