//      This file is part of JACC and is licenced under terms contained in the COPYING file
//      
//      Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

#include <stdio.h>
#include <stdlib.h>

struct st {
    int a;
    int b;
    int c;
    int *p;
};

int main()
{
    int a[100], sum = 0;

    struct st x;

    x.a = 12;
    x.b = 23;
    x.c = 34;
    x.p = malloc(sizeof(int) * 100);

    for (int i = 0; i < 100; i++) {
        x.p[i] = 199+i;
    }

#pragma acc parallel copy (a, x.p[0:100])
    {
#pragma acc loop
        for (int i = 0; i < 100; i++) {
            a[i] = i * x.a + i * i * x.b + x.c + x.p[i];
        }

#pragma acc loop reduction (+:sum)
        for (int i = 0; i < 100; i++) {
            sum += a[i];
        }
    }

    free(x.p);

    printf("%d\n", sum);
    return 0;
}
