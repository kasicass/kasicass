#include <UxFrame.hpp>
#include "Resource.h"

int main()
{
	Ux::WindowPtr wnd = Ux::createWindow<Ux::Window>(IDR_BGIMAGE);
	wnd->show();
	return 0;
}