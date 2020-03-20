//because of library weirdness on some platforms
//if need malloc/calloc - only include the next file
//and don't do anything else
#ifndef sstmac_malloc_included_h
#define sstmac_malloc_included_h

#ifndef SSTMAC_INSIDE_STL
#define SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_clear.h>
#include <sstmac/replacements/clear_symbol_macros.h>
#define STDLIB_OWNS_STL
#endif

#include_next <malloc.h>

#ifdef __cplusplus
extern "C" {
#endif
#pragma sst null_ptr safe
extern void sstmac_free(void* ptr);
#ifdef __cplusplus
}
#endif

#define _mm_free sstmac_free

#ifdef STDLIB_OWNS_STL
#undef STDLIB_OWNS_STL
#undef SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_return.h>
#include <sstmac/replacements/return_symbol_macros.h>
#endif 

#endif

