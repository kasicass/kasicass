#include "UxApp.hpp"
#include <assert.h>

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

namespace Ux {

App* App::s_app = nullptr;

App& App::instance()
{
	assert(s_app);
	return *s_app;
}

App::App()
{
	assert(s_app == nullptr);  // only one Ux::App instance.
	s_app = this;
}

App::~App()
{
}

}

int CALLBACK WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	Ux::App::instance().onInit();
	return 0;
}
