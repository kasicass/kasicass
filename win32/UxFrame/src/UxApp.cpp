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

int CALLBACK WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	Ux::App &app = Ux::App::instance();
	app.setHINSTANCE(hInstance);

	if (app.onInit())
		app.run();

	return 0;
}
