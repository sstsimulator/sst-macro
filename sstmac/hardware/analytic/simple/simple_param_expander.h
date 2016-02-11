#ifndef SIMPLE_PARAM_EXPANDER_H
#define SIMPLE_PARAM_EXPANDER_H

#include <sstmac/hardware/common/param_expander.h>
#include <sprockit/sim_parameters_fwd.h>

namespace sstmac {
namespace hw {

class simple_param_expander :
  public param_expander
{
  public:
    std::string
    to_string() const {
      return "simple param expander";
    }

    virtual void
    expand(sprockit::sim_parameters* params);

  protected:
    void expand_amm1(sprockit::sim_parameters* params);

    void expand_amm2(sprockit::sim_parameters* params);

    void expand_amm3(sprockit::sim_parameters* params);

    void expand_amm4(sprockit::sim_parameters* params);

    void expand_amm1_nic(sprockit::sim_parameters* params);

    void expand_amm1_network(sprockit::sim_parameters* params);

    void expand_amm1_memory(sprockit::sim_parameters* params);

    void expand_amm2_memory(sprockit::sim_parameters* params);

    void expand_amm3_network(sprockit::sim_parameters* params);

    void expand_amm4_nic(sprockit::sim_parameters* params);
};

}
}


#endif // SIMPLE_PARAM_EXPANDER_H
