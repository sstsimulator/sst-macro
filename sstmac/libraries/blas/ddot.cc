#include <sstmac/libraries/blas/blas_api.h>
#include <sprockit/sim_parameters.h>
#include <algorithm>

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

  compute_message*
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

compute_message*
default_ddot::op_1d(int n)
{
  int nops = n;
  long nflops = nops / long(pipeline_);
  long loop_ops = nops / long(loop_unroll_) / long(pipeline_);
  //z = x * y ... 3 vectors
  long total_bytes = 3*n*sizeof(double);

  compute_message* msg = new compute_message;
  msg->set_event_value(compute_message::flop, nflops);
  msg->set_event_value(compute_message::intop, loop_ops);
  msg->set_event_value(compute_message::mem_sequential, total_bytes);
  return msg;
}

}
}




