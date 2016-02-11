#ifndef SSTMAC_SCHED_H
#define SSTMAC_SCHED_H

#include <sstmac/libraries/pthread/sstmac_sys_types.h>
#include <sstmac/libraries/pthread/sstmac_cpu_set.h>
#include <sstmac/libraries/pthread/sstmac_sched_clear_macros.h>
#include <sstmac/libraries/pthread/sstmac_sched_macro.h>

#ifdef __cplusplus
extern "C" {
#endif


/* Set scheduling parameters for a process.  */
extern int SSTMAC_sched_setparam (pid_t pid, const struct sched_param *param);

/* Retrieve scheduling parameters for a particular process.  */
extern int SSTMAC_sched_getparam (pid_t pid, struct sched_param *param);

/* Set scheduling algorithm and/or parameters for a process.  */
extern int SSTMAC_sched_setscheduler (pid_t pid, int policy,  const struct sched_param *param);

/* Retrieve scheduling algorithm for a particular purpose.  */
extern int SSTMAC_sched_getscheduler (pid_t pid);

/* Yield the processor.  */
extern int SSTMAC_sched_yield (void);

/* Get maximum priority value for a scheduler.  */
inline int SSTMAC_sched_get_priority_max (int algorithm){
  return 99;
}

/* Get minimum priority value for a scheduler.  */
inline int SSTMAC_sched_get_priority_min (int algorithm){
  return 1;
}

/* Get the SCHED_RR interval for the named process.  */
extern int SSTMAC_sched_rr_get_interval (pid_t pid, struct timespec *t);

# define SSTMAC_CPU_SETSIZE 64

# define SSTMAC_CPU_SET(cpu, cpusetp)   SSTMAC_CPU_SET_S(cpu, 64, cpusetp)
# define SSTMAC_CPU_CLR(cpu, cpusetp)   SSTMAC_CPU_CLR_S(cpu, 64, cpusetp)
# define SSTMAC_CPU_ISSET(cpu, cpusetp) SSTMAC_CPU_ISSET_S(cpu, 64, cpusetp)
# define SSTMAC_CPU_ZERO(cpusetp)       SSTMAC_CPU_ZERO_S(64, cpusetp)
# define SSTMAC_CPU_COUNT(cpusetp)      SSTMAC_CPU_COUNT_S(64, cpusetp)

void  SSTMAC_CPU_SET_S (int cpu, size_t setsize, sstmac_cpu_set_t* cpusetp);
void  SSTMAC_CPU_CLR_S (int cpu, size_t setsize, sstmac_cpu_set_t* cpusetp);
int   SSTMAC_CPU_ISSET_S (int cpu, size_t setsize, sstmac_cpu_set_t*);
void  SSTMAC_CPU_ZERO_S (size_t setsize, sstmac_cpu_set_t* cpusetp);
int   SSTMAC_CPU_COUNT_S (size_t setsize, sstmac_cpu_set_t* cpusetp);

#define SSTMAC_CPU_EQUAL(cpusetp1, cpusetp2) SSTMAC_CPU_EQUAL_S(64, cpusetp1, cpusetp2)
int SSTMAC_CPU_EQUAL_S(size_t setsize, sstmac_cpu_set_t* cpusetp1, sstmac_cpu_set_t* cpusetp2);

# define SSTMAC_CPU_AND(destset, srcset1, srcset2) SSTMAC_CPU_AND_S(destset, 64, srcset1, srcset2)
# define SSTMAC_CPU_OR(destset, srcset1, srcset2)  SSTMAC_CPU_OR_S(destset, 64, srcset1, srcset2)
# define SSTMAC_CPU_XOR(destset, srcset1, srcset2) SSTMAC_CPU_XOR_S(destset, 64, srcset1, srcset2)

void SSTMAC_CPU_AND_S(size_t setsize, sstmac_cpu_set_t* destset, sstmac_cpu_set_t* srcset1, sstmac_cpu_set_t* srcset2);
void SSTMAC_CPU_OR_S(size_t setsize, sstmac_cpu_set_t* destset, sstmac_cpu_set_t* srcset1, sstmac_cpu_set_t* srcset2);
void SSTMAC_CPU_XOR_S(size_t setsize, sstmac_cpu_set_t* destset, sstmac_cpu_set_t* srcset1, sstmac_cpu_set_t* srcset2);

# define SSTMAC_CPU_ALLOC_SIZE(count) (1<<count)
sstmac_cpu_set_t* SSTMAC_CPU_ALLOC(int count);
void SSTMAC_CPU_FREE(sstmac_cpu_set_t* cpuset);


/* Set the CPU affinity for a task */
extern int SSTMAC_sched_setaffinity (pid_t pid, size_t cpusetsize, const sstmac_cpu_set_t *cpuset);

/* Get the CPU affinity for a task */
extern int SSTMAC_sched_getaffinity (pid_t pid, size_t cpusetsize, sstmac_cpu_set_t *cpuset);

#ifdef __cplusplus
}
#endif

#endif // SSTMAC_SCHED_H
