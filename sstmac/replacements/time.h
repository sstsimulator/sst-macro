
#undef nanosleep

struct timespec;
#include_next <time.h>

#ifdef __cplusplus
extern "C" {
#endif
int sstmac_ts_nanosleep(const struct timespec *req, struct timespec *rem);
#ifdef __cplusplus
}
#endif

#define nanosleep sstmac_ts_nanosleep

