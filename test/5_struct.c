//      This file is part of JACC and is licenced under terms contained in the COPYING file
//      
//      Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

#include <stdio.h>

struct st {
    int a;
    int b;
    int c;
};

int main()
{
    int a[100], sum = 0;

    struct st x;

    x.a = 12;
    x.b = 23;
    x.c = 34;

#pragma acc parallel copy (a)
    {
#pragma acc loop
        for (int i = 0; i < 100; i++) {
            a[i] = i * x.a + i * i * x.b + x.c;
        }

#pragma acc loop reduction (+:sum)
        for (int i = 0; i < 100; i++) {
            sum += a[i];
        }
    }
    printf("%d\n", sum);
    return 0;
}
