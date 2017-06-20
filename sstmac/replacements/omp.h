#ifndef sstmac_replacement_omp_h
#define sstmac_replacement_omp_h

inline static int omp_get_num_threads(){
  return 1;
}

inline static int omp_get_max_threads(){
  return 1;
}

#endif

