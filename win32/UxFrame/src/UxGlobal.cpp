#include "UxGlobal.hpp"

namespace Ux { namespace Global {

static HINSTANCE g_hInst;

HINSTANCE getHINSTANCE()
{
	return g_hInst;
}

void setHINSTANCE(HINSTANCE hInstance)
{
	g_hInst = hInstance;
}

}}