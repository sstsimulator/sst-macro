#ifndef sstmac_software_process_TIME_H
#define sstmac_software_process_TIME_H

#include <sys/time.h>
#define gettimeofday SSTMAC_gettimeofday

#ifdef __cplusplus
extern "C" {
#endif

int SSTMAC_gettimeofday(struct timeval* tv, struct timezone* tz);

double sstmac_wall_time();

#ifdef __cplusplus
}
#endif

#endif // TIME_H
