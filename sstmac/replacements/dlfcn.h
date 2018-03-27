#ifndef sstmac_dlfcn_h_included
#define sstmac_dlfcn_h_included


#include_next <dlfcn.h>

#define dlopen sstmac_dlopen

#ifdef __cplusplus
extern "C" {
#endif

void *sstmac_dlopen(const char *filename, int flag);

#ifdef __cplusplus
}
#endif

#ifdef SIGNAL_H_OWNS_STL
#undef SIGNAL_H_OWNS_STL
#undef SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_return.h>
#endif

#endif

