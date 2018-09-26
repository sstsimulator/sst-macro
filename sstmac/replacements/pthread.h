#ifdef SSTMAC_INSIDE_STL || defined(SSTMAC_NO_REPLACEMENTS)
#include_next <pthread.h>
#else
#include <sstmac/libraries/pthread/sstmac_pthread.h>
#endif

