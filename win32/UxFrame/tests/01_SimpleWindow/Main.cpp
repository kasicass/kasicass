#include <UxFrame.hpp>
#include "Resource.h"

int main(int argc, char* argv[])
{
	Ux::WindowPtr wnd = Ux::createWindow<Ux::Window>(IDR_BGIMAGE);
	wnd->show();
	return 0;
}