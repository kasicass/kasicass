#include "PrintMe.h"
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

PyTypeObject PyType_Type = {
	10, 1.2f,
};

void PrintMe()
{
	printf("PrintMe\n");
}

#ifdef __cplusplus
}
#endif
