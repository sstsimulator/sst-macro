#ifndef SSTMAC_SCHED_MACRO_H
#define SSTMAC_SCHED_MACRO_H

/* Set scheduling parameters for a process.  */
#define sched_setparam SSTMAC_sched_setparam
#define sched_getparam SSTMAC_sched_getparam
#define sched_setscheduler SSTMAC_sched_setscheduler
#define sched_getscheduler SSTMAC_sched_getscheduler
#define sched_yield SSTMAC_sched_yield
#define sched_get_priority_max SSTMAC_sched_get_priority_max
#define sched_get_priority_min SSTMAC_sched_get_priority_min
#define sched_rr_get_interval SSTMAC_sched_rr_get_interval

#define SCHED_FIFO SSTMAC_SCHED_FIFO
#define SCHED_RR SSTMAC_SCHED_RR
#define SCHED_DEADLINE SSTMAC_SCHED_DEADLINE
#define SCHED_OTHER SSTMAC_SCHED_OTHER
#define SCHED_BATCH SSTMAC_SCHED_BATCH
#define SCHED_IDLE SSTMAC_SCHED_IDLE

#define CPU_SETSIZE   SSTMAC_CPU_SETSIZE
#define CPU_SET       SSTMAC_CPU_SET
#define CPU_CLR       SSTMAC_CPU_CLR
#define CPU_ISSET     SSTMAC_CPU_ISSET
#define CPU_ZERO      SSTMAC_CPU_ZERO
#define CPU_COUNT     SSTMAC_CPU_COUNT
#define CPU_SET_S     SSTMAC_CPU_SET_S
#define CPU_CLR_S     SSTMAC_CPU_CLR_S
#define CPU_ISSET_S   SSTMAC_CPU_ISSET_S
#define CPU_ZERO_S    SSTMAC_CPU_ZERO_S
#define CPU_COUNT_S   SSTMAC_CPU_COUNT_S

#define CPU_EQUAL     SSTMAC_CPU_EQUAL
#define CPU_EQUAL_S   SSTMAC_CPU_EQUAL_S
#define CPU_AND       SSTMAC_CPU_AND
#define CPU_OR        SSTMAC_CPU_OR
#define CPU_XOR       SSTMAC_CPU_XOR
#define CPU_AND_S     SSTMAC_CPU_AND_S
#define CPU_OR_S      SSTMAC_CPU_OR_S
#define CPU_XOR_S     SSTMAC_CPU_XOR_S

#define CPU_ALLOC_SIZE SSTMAC_CPU_ALLOC_SIZE
#define CPU_ALLOC     SSTMAC_CPU_ALLOC
#define CPU_FREE      SSTMAC_CPU_FREE

/* Set the CPU affinity for a task */
#define sched_setaffinity SSTMAC_sched_setaffinity
#define sched_getaffinity SSTMAC_sched_getaffinity

#define cpu_set_t sstmac_cpu_set_t

#endif // SSTMAC_SCHED_MACRO_H
