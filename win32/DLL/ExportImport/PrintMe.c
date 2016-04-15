#include "PrintMe.h"
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

int g_MyValue = 10;

void PrintMe()
{
	printf("PrintMe\n");
}

#ifdef __cplusplus
}
#endif
