#include "UxApp.hpp"
#include "UxGdiPlus.hpp"

#if defined(DEBUG) || defined(_DEBUG)
#include <crtdbg.h>
#endif

int CALLBACK WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
#if defined(DEBUG) || defined(_DEBUG)
    _CrtSetDbgFlag( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
#endif

	if (!Ux::gdiPlusInit())
		return 0;

	Ux::App &app = Ux::App::instance();
	app.setHINSTANCE(hInstance);

	if (app.onInit())
		app.run();

	Ux::gdiPlusShutdown();
	return 0;
}
