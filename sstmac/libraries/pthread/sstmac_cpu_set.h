#ifndef SSMAC_CPU_SET_H
#define SSMAC_CPU_SET_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
  uint64_t cpubits;
} sstmac_cpu_set_t;

#ifdef __cplusplus
}
#endif

#endif // SSMAC_CPU_SET_H
