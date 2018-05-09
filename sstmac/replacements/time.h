
#undef nanosleep

#include_next <time.h>

#ifdef __cplusplus
extern "C" {
#endif
int nanosleep(const struct timespec *req, struct timespec *rem);
#ifdef __cplusplus
}
#endif

#define nanosleep sstmac_ts_nanosleep

