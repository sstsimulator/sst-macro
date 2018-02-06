#ifndef sstmac_stdlib_included_h
#define sstmac_stdlib_included_h

#ifndef SSTMAC_INSIDE_STL
#define SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_clear.h>
#include <sstmac/replacements/clear_symbol_macros.h>
#define STDLIB_OWNS_STL
#endif

#include_next <stdlib.h>

#ifndef free
#define free sstmac_free
#endif

#define atexit sstmac_atexit
#define _exit sstmac_exit

#ifdef __cplusplus
extern "C" {
#endif

int sstmac_atexit(void (*)());
void sstmac_exit(int code);
void sstmac_free(void* ptr);

#ifdef __cplusplus
}
#endif

#ifdef STDLIB_OWNS_STL
#undef STDLIB_OWNS_STL
#undef SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_return.h>
#include <sstmac/replacements/return_symbol_macros.h>
#endif

#endif 

