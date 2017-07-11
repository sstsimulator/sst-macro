#ifndef sstmac_replacement_omp_h
#define sstmac_replacement_omp_h

#ifdef __cpluscplus
#define EXTERN extern "C"
#else 
#define EXTERN
#endif

inline static int omp_get_num_threads(){
  return 1;
}

inline static int omp_get_max_threads(){
  return 1;
}

EXTERN double omp_get_wtime();

#undef EXTERN
#endif

