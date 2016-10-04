#ifndef SIMPLE_PARAM_EXPANDER_H
#define SIMPLE_PARAM_EXPANDER_H

#include <sstmac/hardware/common/param_expander.h>
#include <sprockit/sim_parameters_fwd.h>

namespace sstmac {
namespace hw {

class logp_param_expander :
  public param_expander
{
  public:
    virtual void
    expand(sprockit::sim_parameters* params);

  protected:
    void expand_amm1_nic(
      sprockit::sim_parameters* params,
      sprockit::sim_parameters* nic_params,
      sprockit::sim_parameters* switch_params);

    void expand_amm1_network(
      sprockit::sim_parameters* params,
      sprockit::sim_parameters* switch_params);

    void expand_amm1_memory(
      sprockit::sim_parameters* params,
      sprockit::sim_parameters* mem_params);

    void expand_amm2_memory(
      sprockit::sim_parameters* params,
      sprockit::sim_parameters* mem_params);

    void expand_amm3_network(
      sprockit::sim_parameters* params,
      sprockit::sim_parameters* switch_params);

    void expand_amm4_nic(
      sprockit::sim_parameters* params,
      sprockit::sim_parameters* nic_params,
      sprockit::sim_parameters* switch_params);
};

}
}


#endif // SIMPLE_PARAM_EXPANDER_H
