#ifndef PACKET_FLOW_PARAM_EXPANDER_H
#define PACKET_FLOW_PARAM_EXPANDER_H

#include <sstmac/hardware/common/param_expander.h>

namespace sstmac {
namespace hw {

class packet_flow_param_expander :
  public param_expander
{
  public:
    std::string
    to_string() const {
      return "packet flow param expander";
    }

    virtual void
    expand(sprockit::sim_parameters* params);

  private:
    void expand_amm1(sprockit::sim_parameters* params, int net_packet_size, int mem_packet_size);

    void expand_amm2(sprockit::sim_parameters* params, int net_packet_size, int mem_packet_size);

    void expand_amm3(sprockit::sim_parameters* params, int net_packet_size, int mem_packet_size);

    void expand_amm4(sprockit::sim_parameters* params, int net_packet_size, int mem_packet_size);

    void expand_amm1_nic(sprockit::sim_parameters* params, int packet_size);

    void expand_amm1_network(sprockit::sim_parameters* params, int packet_size);

    void expand_amm1_memory(sprockit::sim_parameters* params, int packet_size);

    void expand_amm2_memory(sprockit::sim_parameters* params, int packet_size);

    void expand_amm3_network(sprockit::sim_parameters* params, int packet_size);

    void expand_amm4_nic(sprockit::sim_parameters* params, int packet_size);

    void expand_amm4_network(sprockit::sim_parameters* params, int packet_size);

  private:
    double switch_bandwidth_multiplier(sprockit::sim_parameters *params) const {
      if (tiled_switch_){
        return 1.0;
      } else {
        return param_expander::switch_bandwidth_multiplier(params);
      }
    }

    int switch_buffer_multiplier(sprockit::sim_parameters *params) const {
      if (tiled_switch_){
        return 1;
      } else {
        return param_expander::switch_buffer_multiplier(params);
      }
    }

    double network_bandwidth_multiplier(sprockit::sim_parameters *params) const {
      if (tiled_switch_){
        return 1.0;
      } else {
        return param_expander::switch_bandwidth_multiplier(params);
      }
    }

    double nic_bandwidth_multiplier(sprockit::sim_parameters *params) const {
      if (tiled_switch_){
        return 1.0;
      } else {
        return param_expander::nic_bandwidth_multiplier(params);
      }
    }

    int buffer_depth_;

    bool tiled_switch_;

};

}
}

#endif // PACKET_FLOW_PARAM_EXPANDER_H
