#ifndef SSTMAC_INSIDE_STL
#define SSTMAC_INSIDE_STL

#include <sstmac/replacements/sstmac_pthread_clear.h>
#include <sstmac/replacements/clear_symbol_macros.h>
#include_next <string.h>
#undef SSTMAC_INSIDE_STL
#include <sstmac/replacements/return_symbol_macros.h>
#include <sstmac/replacements/sstmac_pthread_return.h>

#else

#include_next <string.h>

#endif


