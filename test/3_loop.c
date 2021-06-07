//      This file is part of JACC and is licenced under terms contained in the COPYING file
//      
//      Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

#include <stdio.h>

int main()
{
    int a[100], sum = 0;
#pragma acc parallel copy (a)
    {
#pragma acc loop
        for (int i = 0; i < 100; i++) {
            a[i] = i;
        }
#pragma acc loop reduction (+:sum)
        for (int i = 0; i < 100; i++) {
            sum += a[i];
        }
    }
    printf("%d\n", sum);
    return 0;
}
