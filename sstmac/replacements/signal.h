#ifndef SSTMAC_INSIDE_STL
#define SSTMAC_INSIDE_STL

#include <sstmac/replacements/sstmac_pthread_clear.h>

#include_next <signal.h>
#undef SSTMAC_INSIDE_STL

#include <sstmac/replacements/sstmac_pthread_return.h>

#else
#include_next <signal.h>
#endif

