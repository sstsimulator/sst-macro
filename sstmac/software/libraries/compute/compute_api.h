#ifndef sstmac_software_libraries_compute_compute_api_h
#define sstmac_software_libraries_compute_compute_api_h

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief sstmac_sleep SST virtual equivalent of Linux sleep
 * @param secs
 * @return Always zero, successful return code for Linux
 */
int
sstmac_sleep(unsigned int secs);

/**
 * @brief sstmac_usleep SST virtual equivalent of Linux usleep
 * @param usecs
 * @return Always zero, successful return code for Linux
 */
int
sstmac_usleep(unsigned int usecs);

/**
 * @brief sstmac_compute Compute for a specified number of seconds
 * @param secs
 */
void
sstmac_compute(double secs);

/**
 * @brief sstmac_compute_detailed Model a specific compute block
 * @param nflops  The number of flops executed in the compute block
 * @param nintops The number of int ops executed in the compute block
 *
 * @param bytes
 */
void
sstmac_compute_detailed(long nflops, long nintops, long bytes);

/**
 * @brief sstmac_compute_loop
 * @param num_loops        The number of loops to execute
 * @param nflops_per_loop  The number of flops per loop in the inner loop
 * @param nintops_per_loop The number of integer ops in the inner loop (not including loop predicates like i < N)
 * @param bytes_per_loop   The average number of unique bytes read + written per loop
 */
void
sstmac_compute_loop(long num_loops,
                    int nflops_per_loop,
                    int nintops_per_loop,
                    int bytes_per_loop);

/**
 * @brief sstmac_compute_loop2
 * @param isize           The number of indices in the outer loop (imax - imin)
 * @param jsize           The number of indices in the inner loop (jmax - jmin)
 * @param nflops_per_loop  The number of flops per loop in the inner loop
 * @param nintops_per_loop The number of integer ops in the inner loop (not including loop predicates like i < N)
 * @param bytes_per_loop   The average number of unique bytes read + written per loop
 */
void
sstmac_compute_loop2(long isize, long jsize,
                    int nflops_per_loop,
                    int nintops_per_loop,
                    int bytes_per_loop);

/**
 * @brief sstmac_compute_loop3
 * @param isize           The number of indices in the outer loop (imax - imin)
 * @param jsize           The number of indices in the inner loop (jmax - jmin)
 * @param ksize           The number of indices in the inner loop (kmax - kmin)
 * @param nflops_per_loop  The number of flops per loop in the inner loop
 * @param nintops_per_loop The number of integer ops in the inner loop (not including loop predicates like i < N)
 * @param bytes_per_loop   The average number of unique bytes read + written per loop
 */
void
sstmac_compute_loop3(long isize, long jsize,
                    long ksize,
                    int nflops_per_loop,
                    int nintops_per_loop,
                    int bytes_per_loop);

/**
 * @brief sstmac_compute_loop4
 * @param isize           The number of indices in the outer loop (imax - imin)
 * @param jsize           The number of indices in the inner loop (jmax - jmin)
 * @param ksize           The number of indices in the inner loop (kmax - kmin)
 * @param lsize           The number of indices in the inner loop (lmax - lmin)
 * @param nflops_per_loop  The number of flops per loop in the inner loop
 * @param nintops_per_loop The number of integer ops in the inner loop (not including loop predicates like i < N)
 * @param bytes_per_loop   The average number of unique bytes read + written per loop
 */
void
sstmac_compute_loop4(long isize, long jsize,
                    long ksize, long lsize,
                    int nflops_per_loop,
                    int nintops_per_loop,
                    int bytes_per_loop);

void
sstmac_memread(long bytes);

void
sstmac_memwrite(long bytes);

void
sstmac_memcpy(long bytes);

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
