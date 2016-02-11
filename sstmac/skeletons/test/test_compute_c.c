#include <sstmac/software/libraries/compute/sstmac_compute.h>

void
test_compute_c(double time, int blocks)
{
  SSTMAC_compute(time);
  SSTMAC_compute_block_read(blocks);
}
