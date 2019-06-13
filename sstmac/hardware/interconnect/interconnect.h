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
#include <sstmac/common/event_scheduler.h>

#include <sstmac/hardware/common/connection.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/nic/nic_fwd.h>
#include <sstmac/hardware/switch/network_switch_fwd.h>

#include <sstmac/backends/common/parallel_runtime_fwd.h>
#include <sstmac/backends/common/sim_partition_fwd.h>

#include <sprockit/debug.h>
#include <sprockit/factory.h>
#include <unordered_map>

#include <set>

DeclareDebugSlot(interconnect)

namespace sstmac {
namespace hw {

/**
 * Base class for network congestion models.
 */
class Interconnect
{
 public:
  static Interconnect* staticInterconnect(SST::Params& params, EventManager* mgr);

  /**
   * @brief static_interconnect Must already exist
   * @return
   */
  static Interconnect* staticInterconnect();

  static void clearStaticInterconnect(){
    if (static_interconnect_) delete static_interconnect_;
    static_interconnect_ = nullptr;
  }

  Interconnect(SST::Params& params, EventManager* mgr,
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

#if SSTMAC_INTEGRATED_SST_CORE
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

  TimeDelta lookahead() const {
    return lookahead_;
  }

  ConnectableComponent* component(uint32_t id) const {
    return components_[id];
  }

 private:
  uint32_t switchComponentId(SwitchId sid) const;

  uint32_t nodeComponentId(NodeId nid) const;

  uint32_t logpComponentId(SwitchId sid) const;

  /**
   * @brief connectLogP
   * @param idOffset The offset to use for starting to number links
   * @param mgr
   * @param node_params
   * @param nic_params
   * @return The next available link ID offset after connections
   */
  uint64_t connectLogP(uint64_t idOffset, EventManager* mgr,
        SST::Params& node_params, SST::Params& nic_params);

  /**
   * @brief connectSwitches
   * @param idOffset The offset to use for starting to number links
   * @param mgr
   * @param switch_params
   * @return The next available link ID offset after connections
   */
  uint64_t connectSwitches(uint64_t idOffset, EventManager* mgr, SST::Params& switch_params);

  /**
   * @brief connectEndpoints
   * @param idOffset The offset to use for starting to number links
   * @param mgr
   * @param ep_params
   * @param sw_params
   * @return The next available link ID offset after connections
   */
  uint64_t connectEndpoints(uint64_t idOffset, EventManager* mgr, SST::Params& ep_params, SST::Params& sw_params);

  void configureInterconnectLookahead(SST::Params& params);

  void buildEndpoints(SST::Params& node_params,
                    SST::Params& nic_params,
                    EventManager* mgr);

  void buildSwitches(SST::Params& switch_params,
                      EventManager* mgr);

  switch_map switches_;
  node_map nodes_;

  std::vector<ConnectableComponent*> components_;

  TimeDelta lookahead_;

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
