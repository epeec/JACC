#ifndef _JACC_H
#define _JACC_H

#include <stdlib.h>
#include <malloc.h>
#include <openacc.h>

typedef enum {
    __JACC_NONE     = 0,
    __JACC_ARRAY    = 1,
    __JACC_STATIC   = 2,
    __JACC_PRESENT  = 4,
    __JACC_REDUCTED = 8,
    __JACC_WRITTEN  = 16
} __JaccArgAttr;

typedef struct __JaccArg {
    const char *type;

    const char *symbol;

    // variable address (No guarantee data==addr; Can point to allocated mem)
    void *data;

    // array address for arrays and variable address for variables
    void *addr;

    size_t size;

    // array | static | present | reducted | written
    int attr;

    struct __JaccArg *next;
} __JaccArg;

void __jacc_kernel_push(const char *code, __JaccArg *arg);

void __jacc_copyin(void *a, size_t len);

void __jacc_create(void *a, size_t len);

void __jacc_copyout(void *a, size_t len);

void __jacc_delete(void *a, size_t len);

void __jacc_update_self(void *a, size_t len);

void __jacc_update_device(void *a, size_t len);

void *__jacc_malloc(size_t len);

void __jacc_free(void *a);

void __jacc_init();

void __jacc_close();

void __jacc_wait();

void __jacc_acc_init(acc_device_t devicetype);

void __jacc_acc_shutdown(acc_device_t devicetype);

void __jacc_optimize();

#endif
