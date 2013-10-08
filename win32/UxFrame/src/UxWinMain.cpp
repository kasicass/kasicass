#include "UxGlobal.hpp"
#include "UxGdiPlus.hpp"

#if defined(DEBUG) || defined(_DEBUG)
#include <crtdbg.h>
#endif

extern int main(int argc, char* argv[]);
int CALLBACK WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
#if defined(DEBUG) || defined(_DEBUG)
    _CrtSetDbgFlag( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
#endif

	if (!Ux::gdiPlusInit())
		return 0;

	Ux::Global::setHINSTANCE(hInstance);
	main(__argc, __argv);

	Ux::gdiPlusShutdown();
	return 0;
}
