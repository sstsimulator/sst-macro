#ifndef sstmac_unistd_h
#define sstmac_unistd_h

#include_next <unistd.h>

#define gethostname sstmac_gethostname
#define sleep       sstmac_sleep

#ifdef __cplusplus
extern "C" {
#endif
int sstmac_gethostname(const char* name, size_t sz);

unsigned int sstmac_sleep(unsigned int secs);

#ifdef __cplusplus
}
#endif

#endif

