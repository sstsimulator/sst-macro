#include <sstmac/libraries/blas/blas_api.h>
#include <sprockit/sim_parameters.h>
#include <algorithm>
#include <sstmac/software/libraries/compute/compute_event.h>

namespace sstmac {
namespace sw {

class default_ddot :
  public blas_kernel
{
 public:
  std::string
  to_string() const {
    return "default ddot";
  }

  compute_event*
  op_1d(int n);

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

 protected:
  double loop_unroll_;
  double pipeline_;

};
SpktRegister("default_ddot", blas_kernel, default_ddot);

void
default_ddot::init_factory_params(sprockit::sim_parameters* params)
{
  loop_unroll_ = params->get_optional_double_param("ddot_loop_unroll", 4);
  pipeline_ = params->get_optional_double_param("ddot_pipeline_efficiency", 2);
}

compute_event*
default_ddot::op_1d(int n)
{
  basic_compute_event* ev = new basic_compute_event;
  basic_instructions_st& st = ev->data();
  int nops = n;
  st.flops = nops / long(pipeline_);
  st.intops = nops / long(loop_unroll_) / long(pipeline_);
  //z = x * y ... 3 vectors
  st.mem_sequential = 3*n*sizeof(double);
  return ev;
}

}
}




