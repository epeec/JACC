//      This file is part of JACC and is licenced under terms contained in the COPYING file
//      
//      Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

#include <stdio.h>

int main()
{
    int a[100], sum = 0; 
#pragma acc serial copy(a)
    for (int i = 0; i < 100; i++) {
        int j = i % 2;
        a[3] += j + i;
    }
    printf("%d\n", a[3]);
    return 0;
}
