/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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
#include <sstmac/hardware/nic/netlink_fwd.h>
#include <sstmac/hardware/switch/network_switch_fwd.h>

#include <sstmac/backends/common/sim_partition_fwd.h>

#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>
#include <sprockit/unordered.h>

#include <set>

DeclareDebugSlot(interconnect)

#define interconn_debug(...) \
  debug_printf(sprockit::dbg::interconnect, __VA_ARGS__)

namespace sstmac {
namespace hw {

/**
 * Base class for network congestion models.
 */
class interconnect
{
  DeclareFactory(interconnect, event_manager*, partition*, parallel_runtime*)
  FactoryRegister("switch | simple", interconnect, interconnect)
 public:
  static interconnect* static_interconnect(sprockit::sim_parameters* params, event_manager* mgr);

  /**
   * @brief static_interconnect Must already exist
   * @return
   */
  static interconnect* static_interconnect();

  static void
  clear_static_interconnect(){
    if (static_interconnect_) delete static_interconnect_;
    static_interconnect_ = nullptr;
  }

  interconnect(sprockit::sim_parameters* params, event_manager* mgr,
                    partition* part, parallel_runtime* rt);

  int num_nodes() const {
    return num_nodes_;
  }

  switch_id node_to_logp_switch(node_id nid) const;

 protected:
  topology* topology_;
  int num_nodes_;
  int num_switches_;

 private:
  static interconnect* static_interconnect_;

  interconnect(){}

#if SSTMAC_INTEGRATED_SST_CORE
 public:
  bool local_logp_node(node_id nid) const {
    return true;
  }
#else
 public:
  typedef std::vector<network_switch*> switch_map;
  typedef std::vector<connectable*> internal_map;
  typedef std::vector<connectable*> endpoint_map;
  typedef std::vector<node*> node_map;
  typedef std::vector<nic*> nic_map;

  ~interconnect();

  topology* topol() const {
    return topology_;
  }

  inline timestamp send_delay(int num_hops, int num_bytes) const {
    double bw_term = num_bytes / hop_bw_;
    timestamp delay = hop_latency_ * num_hops + timestamp(bw_term) + 2*injection_latency_;
    return delay;
  }

  /**
   * @brief Return the node corresponding to given ID.
   *        No bounds checking is done for validity of ID.
   *        NULL is a valid return value for parallel simulation
   *        since it means node belongs to another process
   * @param nid The ID of the node object to get
   * @return The node object or NULL, if ID is not found
   */
  node* node_at(node_id nid) const {
    return nodes_[nid];
  }

  network_switch* logp_switch_at(switch_id sid) const {
    return logp_overlay_switches_[sid];
  }

  network_switch* switch_at(switch_id id) const {
    return switches_[id];
  }

  const node_map& nodes() const {
    return nodes_;
  }

  void kill_node(node_id nid);

  void kill_node(node_id nid, timestamp t);

  void deadlock_check();

  void handle(event* ev);

  const switch_map& switches() const {
    return switches_;
  }

  int thread_for_switch(switch_id sid) const;

  timestamp hop_latency() const {
    return hop_latency_;
  }

  timestamp lookahead() const {
    return lookahead_;
  }

 private:
  void connect_switches(sprockit::sim_parameters* switch_params);

  void build_endpoints(sprockit::sim_parameters* node_params,
                    sprockit::sim_parameters* nic_params,
                    sprockit::sim_parameters* netlink_params,
                    event_manager* mgr);

  void build_switches(sprockit::sim_parameters* switch_params,
                      event_manager* mgr);

  void connect_endpoints(sprockit::sim_parameters* inj_params,
                  sprockit::sim_parameters* ej_params);

  switch_map switches_;
  //a set of switches that transfer messages quickly
  switch_map logp_overlay_switches_;
  node_map nodes_;

  switch_id local_logp_switch_;

  double hop_bw_;

  timestamp hop_latency_;

  timestamp injection_latency_;

  timestamp lookahead_;

  int num_speedy_switches_with_extra_node_;
  int num_nodes_per_speedy_switch_;

  partition* partition_;
  parallel_runtime* rt_;

  typedef std::vector<netlink*> netlink_map;
  netlink_map netlinks_;
#endif
};

}
} // end of namespace sstmac

#endif