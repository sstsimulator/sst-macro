#include <sstmac/libraries/blas/blas_api.h>
#include <sprockit/sim_parameters.h>
#include <algorithm>
#include <cmath>
#include <sstmac/software/libraries/compute/compute_event.h>

ImplementFactory(sstmac::sw::blas_kernel);

namespace sstmac {
namespace sw {

class default_dgemm :
  public blas_kernel
{
 public:
  default_dgemm(sprockit::sim_parameters* params){
    cache_size_bytes_ = params->get_optional_byte_length_param("dgemm_cache_size", 32000);
    loop_unroll_ = params->get_optional_double_param("dgemm_loop_unroll", 4);
    pipeline_ = params->get_optional_double_param("dgemm_pipeline_efficiency", 2);
  }

  std::string
  to_string() const override {
    return "default dgemm";
  }

  compute_event*
  op_3d(int m, int k, int n) override;

 protected:
  double loop_unroll_;
  double pipeline_;
  int cache_size_bytes_;

};
SpktRegister("default_dgemm", blas_kernel, default_dgemm);

compute_event*
default_dgemm::op_3d(int mm, int nn, int kk)
{
  int sizes[3];
  sizes[0] = mm;
  sizes[1] = kk;
  sizes[2] = nn;
  std::sort(sizes, sizes + 3);

  int k = sizes[0];
  int n = sizes[1];
  int m = sizes[2];

  long Csize = m*n;
  long Asize = m*k;
  long Bsize = k*n;

  int npartitions = 1;
  int nblocks = 1;
  long sum_size = Asize + Bsize + Csize;
  //get to the point where all chunks fit in cache
  while ( (sum_size/nblocks) > cache_size_bytes_ )
  {
    ++npartitions;
    nblocks=npartitions*npartitions;
  }

  basic_compute_event* ev = new basic_compute_event;
  basic_instructions_st& st = ev->data();

  // a single block costs..
  long nops = long(m) * long(n) * long(k);
  // assume we are do a smart Strassen or something - gets better with size
  double exp = 2.807 / 3.0; // log2(7) / log2(8)
  nops = pow(nops, exp);
  st.flops = nops / long(pipeline_);
  st.intops = nops / long(loop_unroll_) / long(pipeline_);
  st.mem_sequential = Csize + Asize*npartitions + Bsize*npartitions;
  return ev;
}

}
}


