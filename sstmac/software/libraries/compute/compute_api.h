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
sstmac_compute_loop(long num_loops, int nflops_per_loop, int nintops_per_loop, int bytes_per_loop);


#ifdef __cplusplus
} //end extern c
#endif

#endif
