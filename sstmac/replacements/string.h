#ifndef sstmac_string_h_included
#define sstmac_string_h_included

#include_next <string.h>

#ifdef SSTMAC_INSIDE_STL
#define sstmac_must_return_memcpy
#define sstmac_must_return_memset
#else
#ifndef memset
#define memset sstmac_memset
#endif
#ifndef memcpy
#define memcpy sstmac_memcpy
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

#pragma sst null_variable safe
void* sstmac_memset(void* ptr, int value, unsigned long  sz);

#pragma sst null_variable safe
void* sstmac_memcpy(void* dst, const void* src, unsigned long sz);

#ifdef __cplusplus
}
#endif


#endif


