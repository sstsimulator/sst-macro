
#if !defined(__need_time_t) && !defined(__need_timespec)

#include_next <time.h>

/** It is maddening that I have to do this
 *  but certain features are not allowed in c99 or older c++
 *  because they declare posix as 1990 */
#include <unistd.h>


/** GCC is a BoD and includes headers in different configs */

#ifndef sstmac_replacement_time_h
#define sstmac_replacement_time_h
#ifdef __cplusplus
extern "C" {
#endif



int sstmac_ts_nanosleep(const struct timespec *req, struct timespec *rem);

#if _POSIX_VERSION > 199309
int SSTMAC_clock_gettime(clockid_t id, struct timespec* ts);
#endif

#ifdef __cplusplus
}
#endif

#ifndef SSTMAC_NO_REPLACEMENTS
#define nanosleep sstmac_ts_nanosleep
#define clock_gettime SSTMAC_clock_gettime
#endif

#endif

#else

#include_next <time.h>

#endif


