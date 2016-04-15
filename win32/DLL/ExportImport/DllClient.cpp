#include "PrintMe.h"
#include <stdio.h>

typedef struct _my_struct {
  int count;
	struct _typeobject *type;
	int size;
} MyStruct;

static MyStruct my = {
	1,
	&PyType_Type,
	2,
};

int main()
{
	printf("v = %d, %f\n", my.type->value, my.type->fvalue);
	return 0;
}

