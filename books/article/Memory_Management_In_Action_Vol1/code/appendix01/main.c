/*-
 * FILE     :  appendix01/main.c
 * PURPOSE  :  a simple sample
 * HISTORY  :  2007-10-16 Created
 *
 * Copyright (C) 2007,2008 kasicass<kasicass@163.com> & buxiu<zhouyang@yahoo.com.cn>
 * All rights reserved.
 */

#include <stdlib.h>

int
main(void)
{
        void *p = NULL;

        p = malloc(10);
        if (p != NULL)
                free(p);

        return 0;
}
