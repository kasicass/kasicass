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

typedef struct _typeobject {
  int value;
  float fvalue;
} PyTypeObject;

PyAPI_DATA(PyTypeObject) PyType_Type;
PyAPI_FUNC(void) PrintMe();

#ifdef __cplusplus
}
#endif

