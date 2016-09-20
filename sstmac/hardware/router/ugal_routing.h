#ifndef sstmac_hardware_network_topology_routing_UGAL_ROUTING_H
#define sstmac_hardware_network_topology_routing_UGAL_ROUTING_H

#include <sstmac/hardware/router/valiant_routing.h>

namespace sstmac {
namespace hw {

/**
 * @brief The ugal_router class
 * Encapsulates a router that performs Univeral Globally Adaptive Load-balanced
 * routing as described in PhD Thesis "Load-balanced in routing in interconnection networks"
 * by A Singh.
 */
class ugal_router :
  public valiant_router
{

 public:
  ugal_router(sprockit::sim_parameters* params, topology* top, network_switch* netsw);

  std::string
  to_string() const override {
    return "ugal";
  }

 protected:
  next_action_t initial_step(
    routable* rtbl,
    packet* pkt) override;

  /**
    The topology object specifies a virtual channel based purely on geometry.
    However, the final virtual channel depends on both geometry and
    routing algorithm.  In this case, we need a separate set of
    virtual channels depending on whether we are in the first
    stage (routing to the intermediate switch) or the second stage
    (routing to the final switch).
    @param topology_vc The geometry-specific virtual channel
    @return The second stage virtual channel
  */
  virtual int
  first_stage_vc(int topology_vc) override {
    return 3 * topology_vc;
  }

  /**
    The topology object specifies a virtual channel based purely on geometry.
    However, the final virtual channel depends on both geometry and
    routing algorithm.  In this case, we need a separate set of
    virtual channels depending on whether we are in the first
    stage (routing to the intermediate switch) or the second stage
    (routing to the final switch).
    @param topology_vc The geometry-specific virtual channel
    @return The second stage virtual channel
  */
  virtual int
  second_stage_vc(int topology_vc) override {
    return 3 * topology_vc + 1;
  }

  /**
    In adition to the valiant stages, ugal has a minimal stage.
    This requires another set of virtual channels.
    @param topology_vc The geometry-specific virtual channel
    @return The zero stage virtual channel
  */
  int
  zero_stage_vc(int topology_vc) {
    return 3 * topology_vc + 2;
  }

 protected:
  int val_threshold_;
  int val_preference_factor_;


};

}
}


#endif // UGAL_ROUTING_H

