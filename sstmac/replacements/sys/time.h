#include_next <sys/time.h>

#ifndef sstmac_replacements_sys_time_h
#define sstmac_replacements_sys_time_h

#ifndef SSTMAC_NO_REPLACEMENTS
#define gettimeofday SSTMAC_gettimeofday
#endif

#ifdef __cplusplus
extern "C" {
#endif
    
int SSTMAC_gettimeofday(struct timeval* tv, struct timezone* tz);

#ifdef __cplusplus
}
#endif

#endif

