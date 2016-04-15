#pragma once

#ifdef MY_BUILD_CODE
#  define PyAPI_FUNC(RTYPE) __declspec(dllexport) RTYPE
#  define PyAPI_DATA(RTYPE) extern __declspec(dllexport) RTYPE
#else
#  define PyAPI_FUNC(RTYPE) __declspec(dllimport) RTYPE
#  define PyAPI_DATA(RTYPE) extern __declspec(dllimport) RTYPE
#endif

#ifdef __cplusplus
extern "C" {
#endif

PyAPI_DATA(int) g_MyValue;
PyAPI_FUNC(void) PrintMe();

#ifdef __cplusplus
}
#endif

