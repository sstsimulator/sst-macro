#ifndef sstmac_string_h_included
#define sstmac_string_h_included

#ifndef SSTMAC_INSIDE_STL
#define SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_clear.h>
#include <sstmac/replacements/clear_symbol_macros.h>
#define STRING_H_OWNS_STL
#endif

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

#pragma sst null_ptr safe
void* sstmac_memset(void* ptr, int value, unsigned long  sz);

#pragma sst null_ptr safe
void* sstmac_memcpy(void* dst, const void* src, unsigned long sz);

#ifdef __cplusplus
}
#endif

#ifdef STRING_H_OWNS_STL
#undef STRING_H_OWNS_STL
#undef SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_return.h>
#include <sstmac/replacements/return_symbol_macros.h>
#endif

#endif


