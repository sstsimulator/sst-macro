#include <sstmac/libraries/blas/blas_api.h>
#include <sprockit/sim_parameters.h>
#include <algorithm>
#include <sstmac/software/libraries/compute/compute_event.h>

namespace sstmac {
namespace sw {

class default_dgemv :
  public blas_kernel
{
 public:
  default_dgemv(sprockit::sim_parameters* params){
    loop_unroll_ = params->get_optional_double_param("dgemv_loop_unroll", 4);
    pipeline_ = params->get_optional_double_param("dgemv_pipeline_efficiency", 2);
  }

  std::string
  to_string() const override {
    return "default dgemv";
  }

  compute_event*
  op_2d(int m, int n) override;

 protected:
  double loop_unroll_;
  double pipeline_;

};
SpktRegister("default_dgemv", blas_kernel, default_dgemv);

compute_event*
default_dgemv::op_2d(int m, int n)
{
  basic_compute_event* msg = new basic_compute_event;
  basic_instructions_st& st = msg->data();

  long nops = long(m) * long(n);
  st.flops= nops / long(pipeline_);
  st.intops = nops / long(loop_unroll_) / long(pipeline_);
  st.mem_sequential = long(m)*long(n)*sizeof(double);
  return msg;
}

}
}



