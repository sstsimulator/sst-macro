#ifndef sstmac_hardware_network_topology_routing_VALIANT_ROUTER_H
#define sstmac_hardware_network_topology_routing_VALIANT_ROUTER_H

#include <sstmac/hardware/router/minimal_routing.h>

namespace sstmac {
namespace hw {

/**
 * @brief The valiant_router class
 * Enacpsulates a router that performs oblivious, global routing according to
 * "A Scheme for Fast Parallel Computation" by Valiant.
 */
class valiant_router : public minimal_router
{
 public:
  valiant_router() :
    minimal_router(routing::valiant){}

  virtual
  ~valiant_router() { }

  virtual void
  route(packet* pkt);

  virtual void
  route(packet* pkt, structured_routable::path_set& paths);

  virtual std::string
  to_string() const {
    return "valiant";
  }

  virtual void
  finalize_init();

  /**
    The topology object specifies a virtual channel based purely on geometry.
    However, the final virtual channel depends on both geometry and
    routing algorithm.  In this case, we need a separate set of
    virtual channels depending on whether we are in the first
    stage (routing to the intermediate switch) or the second stage
    (routing to the final switch).
    @param topology_vc The geometry-specific virtual channel
    @return The first stage virtual channel
  */
  virtual int
  first_stage_vc(int topology_vc) {
    return 2*topology_vc;
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
  second_stage_vc(int topology_vc) {
    return 2*topology_vc + 1;
  }

 protected:
  valiant_router(routing::algorithm_t algo) :
    minimal_router(algo){}

  typedef enum {
    minimal,
    intermediate_switch,
    final_node
  } next_action_t;

  /**
   * @brief intermediate_step
   * @param rtbl  The routing interface with path information
   * @param pkt   The packet corresponding to the structured_routable
   * @return The next action to take. It may be intermediate_switch or
   * final_node depending on whether the intermediate switch has been
   * reached or not.
   */
  next_action_t
  intermediate_step(structured_routable* rtbl,
                    packet* pkt);

  /**
   Different for pure valiant and UGAL.
   */
  virtual next_action_t
  initial_step(structured_routable* rtbl,
               packet* pkt);

  /**
   * @brief next_routing_stage
   * Figure out which action is required for the packet
   * @param pkt The packet being routed
   * @return It may be intermediate_switch or
   * final_node depending on whether the intermediate switch has been
   * reached or not. Or it may be minimal if the packet is still on
   * the minimal path and has not switched to Valiant.
   */
  next_action_t
  next_routing_stage(packet* pkt);

  /**
   * @brief configure_intermediate_path
   * Once the packet has switched to Valiant, compute a path to the
   * chosen intermediate switch
   * @param path
   */
  void
  configure_intermediate_path(structured_routable::path& path);

  /**
   * @brief configure_final_path
   * Once the packet has switched to Valiant and reached the intermediate switch,
   * compute a path to the final ejection switch
   * @param path
   */
  void
  configure_final_path(structured_routable::path& path);

  /**
   * @brief route_valiant
   * Figure out if in intermediate or final stage and make appropriate routing computation
   * @param pkt
   */
  void route_valiant(packet* pkt);

};

}
}

#endif // VALIANT_ROUTER_H

