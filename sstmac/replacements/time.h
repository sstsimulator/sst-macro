#ifndef sstmac_time_h_included
#define sstmac_time_h_included

#ifndef SSTMAC_INSIDE_STL
#define SSTMAC_INSIDE_STL
#define SIGNAL_H_OWNS_STL
#include <sstmac/replacements/sstmac_pthread_clear.h>
#endif

#include_next <time.h>

#ifdef __cplusplus
extern "C" {
#endif
int nanosleep(const struct timespec *req, struct timespec *rem);
#ifdef __cplusplus
}
#endif
#define nanosleep sstmac_ts_nanosleep

#ifdef SIGNAL_H_OWNS_STL
#undef SIGNAL_H_OWNS_STL
#undef SSTMAC_INSIDE_STL
#include <sstmac/replacements/sstmac_pthread_return.h>
#endif

#endif

