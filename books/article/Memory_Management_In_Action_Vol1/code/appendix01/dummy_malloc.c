/*-
 * FILE     :  appendix01/dummy_alloc.c
 * PURPOSE  :  a dummy implementation of malloc()
 * HISTORY  :  2007-10-16 Created
 *
 * Copyright (C) 2007,2008 kasicass<kasicass@163.com> & buxiu<zhouyang@yahoo.com.cn>
 * All rights reserved.
 */

#include <stdio.h>

void *
malloc(size_t size)
{
        printf("malloc(%d) ...\n", (int)size);
        return NULL;    // simulate no-memory
}
