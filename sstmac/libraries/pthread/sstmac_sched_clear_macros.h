#ifndef SSTMAC_SCHED_CLEAR_MACROS_H
#undef SSTMAC_SCHED_CLEAR_MACROS_H

/* Set scheduling parameters for a process.  */
#undef sched_setparam
#undef sched_getparam
#undef sched_setscheduler
#undef sched_getscheduler
#undef sched_yield
#undef sched_get_priority_max
#undef sched_get_priority_min
#undef sched_rr_get_interval

#undef SCHED_FIFO
#undef SCHED_RR
#undef SCHED_DEADLINE
#undef SCHED_OTHER
#undef SCHED_BATCH
#undef SCHED_IDLE

#undef CPU_SETSIZE
#undef CPU_SET
#undef CPU_CLR
#undef CPU_ISSET
#undef CPU_ZERO
#undef CPU_COUNT
#undef CPU_SET_S
#undef CPU_CLR_S
#undef CPU_ISSET_S
#undef CPU_ZERO_S
#undef CPU_COUNT_S

#undef CPU_EQUAL
#undef CPU_EQUAL_S
#undef CPU_AND
#undef CPU_OR
#undef CPU_XOR
#undef CPU_AND_S
#undef CPU_OR_S
#undef CPU_XOR_S

#undef CPU_ALLOC_SIZE
#undef CPU_ALLOC
#undef CPU_FREE

/* Set the CPU affinity for a task */
#undef sched_setaffinity
#undef sched_getaffinity

#undef cpu_set_t

#endif // SSTMAC_SCHED_CLEAR_MACROS_H
