#include "UxGdiPlus.hpp"

#include <Windows.h>
#include <GdiPlus.h>
#include <assert.h>

namespace Ux {

static ULONG_PTR s_token;

bool gdiPlusInit()
{
	Gdiplus::GdiplusStartupInput input;
	Gdiplus::Status ret = Gdiplus::GdiplusStartup(&s_token, &input, NULL);
	return (ret == Gdiplus::Ok);
}

void gdiPlusShutdown()
{
	Gdiplus::GdiplusShutdown(s_token);
}

}
