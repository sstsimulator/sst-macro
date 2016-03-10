#include <sstmac/libraries/blas/blas_api.h>
#include <sprockit/sim_parameters.h>
#include <algorithm>

namespace sstmac {
namespace sw {

class default_daxpy :
  public blas_kernel
{
 public:
  std::string
  to_string() const {
    return "default daxpy";
  }

  compute_message*
  op_1d(int n);

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

 protected:
  double loop_unroll_;
  double pipeline_;

};
SpktRegister("default_daxpy", blas_kernel, default_daxpy);

void
default_daxpy::init_factory_params(sprockit::sim_parameters* params)
{
  loop_unroll_ = params->get_optional_double_param("daxpy_loop_unroll", 4);
  pipeline_ = params->get_optional_double_param("daxpy_pipeline_efficiency", 2);
}

compute_message*
default_daxpy::op_1d(int n)
{
  int nops = n;
  long nflops = nops / long(pipeline_);
  long loop_ops = nops / long(loop_unroll_) / long(pipeline_);
  // y += alpha * x ... 2 vectors
  long total_bytes = 2*n*sizeof(double);

  compute_message* msg = new compute_message;
  msg->set_event_value(compute_message::flop, nflops);
  msg->set_event_value(compute_message::intop, loop_ops);
  msg->set_event_value(compute_message::mem_sequential, total_bytes);
  return msg;
}

}
}



