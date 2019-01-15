/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#ifndef SSTMAC_HARDWARE_NETWORK_CONGESTION_INTERCONNECT_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_CONGESTION_INTERCONNECT_H_INCLUDED

#include <sstmac/common/timestamp.h>
#include <sstmac/common/node_address.h>

#include <sstmac/hardware/common/connection.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/nic/nic_fwd.h>
#include <sstmac/hardware/switch/network_switch_fwd.h>

#include <sstmac/backends/common/sim_partition_fwd.h>

#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>
#include <unordered_map>

#include <set>

DeclareDebugSlot(interconnect)

#define interconn_debug(...) \
  debug_printf(sprockit::dbg::interconnect, __VA_ARGS__)

namespace sstmac {
namespace hw {

/**
 * Base class for network congestion models.
 */
class Interconnect
{
  DeclareFactory(Interconnect, EventManager*, Partition*, ParallelRuntime*)
  FactoryRegister("switch | simple", Interconnect, Interconnect)
 public:
  static Interconnect* staticInterconnect(sprockit::sim_parameters::ptr& params, EventManager* mgr);

  /**
   * @brief static_interconnect Must already exist
   * @return
   */
  static Interconnect* staticInterconnect();

  static void clear_staticInterconnect(){
    if (static_interconnect_) delete static_interconnect_;
    static_interconnect_ = nullptr;
  }

  Interconnect(sprockit::sim_parameters::ptr& params, EventManager* mgr,
                    Partition* part, ParallelRuntime* rt);

  int numNodes() const {
    return num_nodes_;
  }

  SwitchId nodeToLogpSwitch(NodeId nid) const;

 protected:
  Topology* topology_;
  int num_nodes_;
  int num_switches_;
  int num_leaf_switches_;

 private:
  static Interconnect* static_interconnect_;

  Interconnect(){}

#if ACTUAL_INTEGRATED_SST_CORE
#else
 public:
  const std::vector<ConnectableComponent*>& components() const {
    return components_;
  }

  typedef std::vector<NetworkSwitch*> switch_map;
  typedef std::vector<Connectable*> internal_map;
  typedef std::vector<Connectable*> endpoint_map;
  typedef std::vector<Node*> node_map;
  typedef std::vector<NIC*> nic_map;

  ~Interconnect();

  Topology* topology() const {
    return topology_;
  }

  /**
   * @brief Return the node corresponding to given ID.
   *        No bounds checking is done for validity of ID.
   *        NULL is a valid return value for parallel simulation
   *        since it means node belongs to another process
   * @param nid The ID of the node object to get
   * @return The node object or NULL, if ID is not found
   */
  Node* nodeAt(NodeId nid) const {
    return nodes_[nid];
  }

  NetworkSwitch* switchAt(SwitchId id) const {
    return switches_[id];
  }

  const node_map& nodes() const {
    return nodes_;
  }

  void setup();

  const switch_map& switches() const {
    return switches_;
  }

  Timestamp lookahead() const {
    return lookahead_;
  }

  ConnectableComponent* component(uint32_t id) const {
    return components_[id];
  }

 private:
  uint32_t switchComponentId(SwitchId sid) const;

  uint32_t nodeComponentId(NodeId nid) const;

  uint32_t logpComponentId(SwitchId sid) const;

  void connectLogP(EventManager* mgr,
        sprockit::sim_parameters::ptr& node_params,
        sprockit::sim_parameters::ptr& nic_params);

  void connectSwitches(EventManager* mgr, sprockit::sim_parameters::ptr& switch_params);

  void configureInterconnectLookahead(sprockit::sim_parameters::ptr& params);

  void buildEndpoints(sprockit::sim_parameters::ptr& node_params,
                    sprockit::sim_parameters::ptr& nic_params,
                    EventManager* mgr);

  void buildSwitches(sprockit::sim_parameters::ptr& switch_params,
                      EventManager* mgr);

  void connectEndpoints(EventManager* mgr,
                  sprockit::sim_parameters::ptr& ep_inj_params,
                  sprockit::sim_parameters::ptr& ep_ej_params,
                  sprockit::sim_parameters::ptr& sw_ej_params);


  switch_map switches_;

  node_map nodes_;

  std::vector<ConnectableComponent*> components_;

  Timestamp lookahead_;

  int num_speedy_switches_with_extra_node_;
  int num_nodes_per_speedy_switch_;

  std::vector<LogPSwitch*> logp_switches_;

  Partition* partition_;
  ParallelRuntime* rt_;
#endif
};

}
} // end of namespace sstmac

#endif
