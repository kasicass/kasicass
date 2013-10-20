#pragma once

#define STRICT
#define WIN32_LEAN_AND_MEAN
#define _WTL_USE_CSTRING

#include <atlbase.h>         // base ATL classes
#include <atlapp.h>          // base WTL classes
extern CAppModule _Module;   // WTL version of CComModule
#include <atlwin.h>          // ATL GUI classes
#include <atlframe.h>        // WTL frame window classes
#include <atlmisc.h>         // WTL utility classes like CString
#include <atlcrack.h>        // WTL enhanced msg map macros

#include "resource.h"