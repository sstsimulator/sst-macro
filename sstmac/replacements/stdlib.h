
#ifndef SSTMAC_INSIDE_STL
#define SSTMAC_INSIDE_STL

#include <sstmac/replacements/sstmac_pthread_clear.h>
#include <sstmac/replacements/clear_symbol_macros.h>
#include_next <stdlib.h>
#undef SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_return.h>
#include <sstmac/replacements/return_symbol_macros.h>

#else

#include_next <stdlib.h>

#define atexit sstmac_atexit
#define _exit sstmac_exit

#ifdef __cplusplus
extern "C" {
#endif

int sstmac_atexit(void (*)());
void sstmac_exit(int code);

#ifdef __cplusplus
}
#endif

#endif


