//      This file is part of JACC and is licenced under terms contained in the COPYING file
//      
//      Copyright (C) 2021 Barcelona Supercomputing Center (BSC)

#include <stdio.h>
#include <stdlib.h>

void f(int *a, int len)
{
    a[48]=123;
#pragma acc enter data copyin(a[0:len])
    a[47]=456;
}

void g(int *a, int len)
{
    a[49]=1;
#pragma acc exit data copyout(a[0:len])
    a[49]=1;
}

int main()
{
    int a[100], sum = 0;

    f(a, 100);
#pragma acc parallel
#pragma acc loop reduction (+:sum)
    for (int i = 0; i < 100; i++) {
        a[i] = i * i;
        sum += i;
    }
    g(a, 100);

    printf("%d\n", sum + a[47] + a[48] + a[49] + a[50]);
    return 0;
}
