#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/libraries/compute/compute_api.h>

extern "C" int
sstmac_sleep(unsigned int secs){
  sstmac::sw::operating_system::current_thread()->parent_app()->sleep(sstmac::timestamp(secs));
  return 0;
}

extern "C" int
sstmac_usleep(unsigned int usecs){
  sstmac::sw::operating_system::current_thread()->parent_app()->sleep(sstmac::timestamp(usecs*1e-6));
  return 0;
}

extern "C" void
sstmac_compute(double secs){
  sstmac::sw::operating_system::current_thread()->parent_app()->compute(sstmac::timestamp(secs));
}

extern "C" void
sstmac_memread(long bytes){
  sstmac::sw::operating_system::current_thread()->parent_app()
    ->compute_block_read(bytes);
}

extern "C" void
sstmac_memwrite(long bytes){
  sstmac::sw::operating_system::current_thread()->parent_app()
    ->compute_block_write(bytes);
}

extern "C" void
sstmac_memcpy(long bytes){
  sstmac::sw::operating_system::current_thread()->parent_app()
    ->compute_block_memcpy(bytes);
}

extern "C" void
sstmac_compute_detailed(long nflops, long nintops, long bytes){
  sstmac::sw::operating_system::current_thread()->parent_app()
    ->compute_detailed(nflops, nintops, bytes);
}

extern "C" void
sstmac_compute_loop(long num_loops, int nflops_per_loop, int nintops_per_loop, int bytes_per_loop){
  sstmac::sw::operating_system::current_thread()->parent_app()
    ->compute_loop(num_loops, nflops_per_loop, nintops_per_loop, bytes_per_loop);
}
