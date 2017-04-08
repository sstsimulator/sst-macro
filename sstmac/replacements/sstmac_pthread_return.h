
#ifdef SSTMAC_PTHREAD_MACRO_H 
//if sstmac pthread included, bring it back
#undef SSTMAC_PTHREAD_MACRO_H
#undef PTHREAD_COND_INITIALIZER
#undef PTHREAD_MUTEX_INITIALIZER
#include <sstmac/libraries/pthread/sstmac_pthread_macro.h>
#endif
