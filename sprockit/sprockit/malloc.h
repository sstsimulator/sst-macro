#ifndef sprockit_common_MALLOC_H
#define sprockit_common_MALLOC_H

#include <stdlib.h>

#define SPROCKIT_FAKE_PTR ((void*)0x123)

#ifdef SSTMAC

#ifdef __cplusplus
#define large_new(type,size) ((type*)SPROCKIT_FAKE_PTR)
#endif
#define large_malloc(size) SPROCKIT_FAKE_PTR

#else

#ifdef __cplusplus
#define large_new(type,size) new type[size];
#define large_malloc(size) ::malloc(size)
#else
#define large_malloc(size) malloc(size)
#endif

#endif


#endif // MALLOC_H

