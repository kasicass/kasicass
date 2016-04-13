#include <stdio.h>

extern int foovalue;

int main()
{
	int *p = &foovalue;
	printf("%d\n", *p);
	return 0;
}

