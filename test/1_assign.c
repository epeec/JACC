//      This file is part of JACC and is licenced under terms contained in the COPYING file
//
//      Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

#include <stdio.h>

int main()
{
    int a[100], sum = 0;
#pragma acc parallel copy (a)
    {
        a[0] = 1;
    }
    printf("%d\n", a[0]);
    return 0;
}
