#ifndef sstmac_string_h_included
#define sstmac_string_h_included

#ifndef SSTMAC_INSIDE_STL
#define SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_clear.h>
#include <sstmac/replacements/clear_symbol_macros.h>
#define STRING_OWNS_STL
#endif

#include_next <string.h>

#ifndef memset
#define memset sstmac_memset
#endif

#ifdef __cplusplus
extern "C" {
#endif

void* sstmac_memset(void* ptr, int value, unsigned long  sz);

#ifdef __cplusplus
}
#endif

#ifdef STRING_OWNS_STL
#undef STRING_OWNS_STL
#undef SSTMAC_INSIDE_STL
#include <sstmac/replacements/return_symbol_macros.h>
#include <sstmac/replacements/sstmac_pthread_return.h>
#endif

#endif


