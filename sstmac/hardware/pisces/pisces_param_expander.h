#ifndef pisces_PARAM_EXPANDER_H
#define pisces_PARAM_EXPANDER_H

#include <sstmac/hardware/common/param_expander.h>

namespace sstmac {
namespace hw {

class pisces_param_expander :
  public param_expander
{
  public:
    virtual void
    expand(sprockit::sim_parameters* params) override;

  private:
    void expand_amm1_nic(sprockit::sim_parameters* params,
                         sprockit::sim_parameters* nic_params);

    void expand_amm1_network(sprockit::sim_parameters* params,
                             sprockit::sim_parameters* switch_params,
                             bool set_xbar);

    void expand_amm1_memory(sprockit::sim_parameters* params,
                            sprockit::sim_parameters* mem_params);

    void expand_amm2_memory(sprockit::sim_parameters* params,
                            sprockit::sim_parameters* mem_params);

    void expand_amm3_network(sprockit::sim_parameters* params,
                             sprockit::sim_parameters* switch_params);

    void expand_amm4_nic(sprockit::sim_parameters* params,
                         sprockit::sim_parameters* top_params,
                         sprockit::sim_parameters* nic_params);

    void expand_amm4_network(sprockit::sim_parameters* params,
                             sprockit::sim_parameters* top_params,
                             sprockit::sim_parameters* nic_params);

  private:
    double switch_bandwidth_multiplier(sprockit::sim_parameters *params) const override {
      if (tiled_switch_){
        return 1.0;
      } else {
        return param_expander::switch_bandwidth_multiplier(params);
      }
    }

    int switch_buffer_multiplier(sprockit::sim_parameters *params) const override {
      if (tiled_switch_){
        return 1;
      } else {
        return param_expander::switch_buffer_multiplier(params);
      }
    }

    double network_bandwidth_multiplier(sprockit::sim_parameters *params) const override {
      if (tiled_switch_){
        return 1.0;
      } else {
        return param_expander::network_bandwidth_multiplier(params);
      }
    }

    int buffer_depth_;

    bool tiled_switch_;

};

}
}

#endif // pisces_PARAM_EXPANDER_H
