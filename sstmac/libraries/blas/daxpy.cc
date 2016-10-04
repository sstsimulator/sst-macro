#include <sstmac/libraries/blas/blas_api.h>
#include <sprockit/sim_parameters.h>
#include <algorithm>
#include <sstmac/software/libraries/compute/compute_event.h>

namespace sstmac {
namespace sw {

class default_daxpy :
  public blas_kernel
{
 public:
  default_daxpy(sprockit::sim_parameters* params){
    loop_unroll_ = params->get_optional_double_param("daxpy_loop_unroll", 4);
    pipeline_ = params->get_optional_double_param("daxpy_pipeline_efficiency", 2);
  }

  std::string
  to_string() const override {
    return "default daxpy";
  }

  compute_event*
  op_1d(int n) override;

 protected:
  double loop_unroll_;
  double pipeline_;

};
SpktRegister("default_daxpy", blas_kernel, default_daxpy);

compute_event*
default_daxpy::op_1d(int n)
{
  basic_compute_event* ev = new basic_compute_event;
  basic_instructions_st& st = ev->data();
  int nops = n;
  st.flops = nops / long(pipeline_);
  st.intops = nops / long(loop_unroll_) / long(pipeline_);
  // y += alpha * x ... 2 vectors
  st.mem_sequential = 2*n*sizeof(double);
  return ev;
}

}
}



