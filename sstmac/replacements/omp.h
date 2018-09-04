#ifndef sstmac_replacement_omp_h
#define sstmac_replacement_omp_h


#define omp_init_lock sstmac_omp_init_lock
#define omp_destroy_lock sstmac_omp_destroy_lock
#define omp_set_lock sstmac_omp_set_lock
#define omp_unset_lock sstmac_omp_unset_lock
#define omp_test_lock sstmac_omp_test_lock
#define omp_get_num_threads sstmac_omp_get_num_threads
#define omp_get_thread_num sstmac_omp_get_thread_num
#define omp_get_max_threads sstmac_omp_get_max_threads
#define omp_get_wtime sstmac_omp_get_wtime
#define omp_get_num_procs sstmac_omp_get_num_procs
#define omp_set_num_threads sstmac_omp_set_num_threads
#define omp_in_parallel sstmac_omp_in_parallel
#define omp_get_level sstmac_omp_get_level
#define omp_get_ancestor_thread_num sstmac_omp_get_ancestor_thread_num


#define sstmac_omp_lock_t int

#ifdef __cplusplus
extern "C" {
#endif

void sstmac_omp_init_lock(sstmac_omp_lock_t *lock);

void sstmac_omp_destroy_lock(sstmac_omp_lock_t *lock);

void sstmac_omp_set_lock(sstmac_omp_lock_t *lock);

void sstmac_omp_unset_lock(sstmac_omp_lock_t *lock);

int sstmac_omp_test_lock(sstmac_omp_lock_t *lock);

int sstmac_omp_get_thread_num();

int sstmac_omp_get_num_procs();

int sstmac_omp_get_num_threads();

int sstmac_omp_get_max_threads();

void sstmac_omp_set_num_threads(int nthr);

int sstmac_omp_in_parallel();

int sstmac_omp_get_level();

int sstmac_omp_get_ancestor_thread_num();

double sstmac_omp_get_wtime();

#define omp_lock_t sstmac_omp_lock_t

#ifdef __cplusplus
}
#endif

#endif

