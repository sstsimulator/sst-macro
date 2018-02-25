#ifndef sstmac_replacement_omp_h
#define sstmac_replacement_omp_h

#ifdef __cpluscplus
#define EXTERN extern "C"
#else 
#define EXTERN
#endif


#define omp_init_lock sstmac_omp_init_lock
#define omp_destroy_lock sstmac_omp_destroy_lock
#define omp_set_lock sstmac_omp_set_lock
#define omp_unset_lock sstmac_omp_unset_lock
#define omp_test_lock sstmac_omp_test_lock
#define omp_get_num_threads sstmac_omp_get_num_threads
#define omp_get_max_threads sstmac_omp_get_max_threads
#define omp_get_wtime sstmac_omp_get_wtime
#define sstmac_omp_lock_t int

void sstmac_omp_init_lock(sstmac_omp_lock_t *lock);

void sstmac_omp_destroy_lock(sstmac_omp_lock_t *lock);

void sstmac_omp_set_lock(sstmac_omp_lock_t *lock);

void sstmac_omp_unset_lock(sstmac_omp_lock_t *lock);

void sstmac_omp_test_lock(sstmac_omp_lock_t *lock);

inline static int sstmac_omp_get_num_threads(){
  return 1;
}

inline static int sstmac_omp_get_max_threads(){
  return 1;
}

EXTERN double sstmac_omp_get_wtime();

#define omp_lock_t sstmac_omp_lock_t

#undef EXTERN
#endif

