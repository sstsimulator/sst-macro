#ifndef sstmac_software_libraries_compute_compute_api_h
#define sstmac_software_libraries_compute_compute_api_h

#ifdef __cplusplus
extern "C" {
#endif

int
sstmac_sleep(unsigned int secs);

int
sstmac_usleep(unsigned int usecs);

void
sstmac_compute(double secs);

void
sstmac_memread(long bytes);

void
sstmac_memwrite(long bytes);

void
sstmac_memcpy(long bytes);

void
sstmac_compute_detailed(long nflops, long nintops, long bytes);

void
sstmac_compute_loop(long num_loops,
                    int nflops_per_loop,
                    int nintops_per_loop,
                    int bytes_per_loop);

void
sstmac_compute_loop2(long isize, long jsize,
                    int nflops_per_loop,
                    int nintops_per_loop,
                    int bytes_per_loop);

void
sstmac_compute_loop3(long isize, long jsize,
                    long ksize,
                    int nflops_per_loop,
                    int nintops_per_loop,
                    int bytes_per_loop);

void
sstmac_compute_loop4(long isize, long jsize,
                    long ksize, long lsize,
                    int nflops_per_loop,
                    int nintops_per_loop,
                    int bytes_per_loop);

#define SSTMAC_sleep(...) sstmac_sleep(__VA_ARGS__)
#define SSTMAC_usleep(...) sstmac_usleep(__VA_ARGS__)
#define SSTMAC_compute(...) sstmac_compute(__VA_ARGS__)
#define SSTMAC_memread(...) sstmac_memread(__VA_ARGS__)
#define SSTMAC_memwrite(...) sstmac_memwrite(__VA_ARGS__)
#define SSTMAC_memcpy(...) sstmac_memcpy(__VA_ARGS__)
#define SSTMAC_compute_detailed(...) sstmac_compute_detailed(__VA_ARGS__)
#define SSTMAC_compute_loop(...) sstmac_compute_loop(__VA_ARGS__)
#define SSTMAC_compute_loop2(...) sstmac_compute_loop2(__VA_ARGS__)
#define SSTMAC_compute_loop3(...) sstmac_compute_loop3(__VA_ARGS__)
#define SSTMAC_compute_loop4(...) sstmac_compute_loop4(__VA_ARGS__)

#ifdef __cplusplus
} //end extern c
#endif

#endif
