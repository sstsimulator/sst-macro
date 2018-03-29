#ifndef sstmac_signal_h_included
#define sstmac_signal_h_included

#ifndef SSTMAC_INSIDE_STL
#define SSTMAC_INSIDE_STL
#define SIGNAL_H_OWNS_STL
#include <sstmac/replacements/sstmac_pthread_clear.h>
#endif

#include_next <signal.h>

#ifdef SIGNAL_H_OWNS_STL
#undef SIGNAL_H_OWNS_STL
#undef SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_return.h>
#endif

#endif

