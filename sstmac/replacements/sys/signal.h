#ifndef SSTMAC_INSIDE_STL
#define SSTMAC_INSIDE_STL

#ifdef SSTMAC_PTHREAD_MACRO_H 
//if sstmac pthread included, clear it
#include <sstmac/libraries/pthread/sstmac_pthread_clear_macros.h>
#endif

#include_next <sys/signal.h>
#undef SSTMAC_INSIDE_STL


#ifdef SSTMAC_PTHREAD_MACRO_H 
//if sstmac pthread included, bring it back
#undef SSTMAC_PTHREAD_MACRO_H
#include <sstmac/libraries/pthread/sstmac_pthread_macro.h>
#endif

#else
#include_next <sys/signal.h>
#endif

