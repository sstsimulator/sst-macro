#ifndef sstmac_unistd_h
#define sstmac_unistd_h

#include_next <unistd.h>

#ifndef SSTMAC_NO_REPLACEMENTS
#define sleep       sstmac_sleep
#endif

#define gethostname sstmac_gethostname
#define alarm       sstmac_alarm

#ifdef __cplusplus
extern "C" {
#endif
int sstmac_gethostname(const char* name, size_t sz);

unsigned int sstmac_sleep(unsigned int secs);

unsigned int sstmac_sleepUntil(double t);

unsigned int sstmac_alarm(unsigned int);

#ifdef __cplusplus
}
#endif


#endif

