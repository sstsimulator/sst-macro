//because of library weirdness on some platforms
//if need malloc/calloc - only include the next file
//and don't do anything else
#ifndef sstmac_stdlib_included_h
#ifndef __need_malloc_and_calloc
#define sstmac_stdlib_included_h

#ifndef SSTMAC_INSIDE_STL
#define SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_clear.h>
#include <sstmac/replacements/clear_symbol_macros.h>
#define STDLIB_OWNS_STL
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

//gcc is an abomination, which requires these functions to be available
//because stdlib.h can turn around and include cstdlib
//because gcc is an abomination
int sstmac_atexit(void (*)());
int sstmac_on_exit(void (*)(int,void*),void*);
void sstmac_exit(int code);
extern void sstmac_free(void* ptr);

char* sstmac_getenv(const char* name);
int sstmac_putenv(char* input);
int sstmac_setenv(const char* name, const char* val, int overwrite);

#ifdef __cplusplus
}
#endif

#include_next <stdlib.h>

#pragma sst null_variable safe
void free(void* ptr);

#ifndef __need_malloc_and_calloc
#define atexit sstmac_atexit
#define _exit sstmac_exit
#define on_exit sstmac_on_exit
#define getenv sstmac_getenv
#define setenv sstmac_setenv
#define putenv sstmac_putenv



#ifdef STDLIB_OWNS_STL
#undef STDLIB_OWNS_STL
#undef SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_return.h>
#include <sstmac/replacements/return_symbol_macros.h>
#endif

#endif 
#endif

