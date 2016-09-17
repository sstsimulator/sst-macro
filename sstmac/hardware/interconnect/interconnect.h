/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
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
 public:
  static interconnect*
  static_interconnect(sprockit::sim_parameters* params, event_manager* mgr);

  static void
  clear_static_interconnect(){
    if (static_interconnect_) delete static_interconnect_;
    static_interconnect_ = nullptr;
  }

  interconnect(sprockit::sim_parameters* params, event_manager* mgr,
                    partition* part, parallel_runtime* rt);

  int
  num_nodes() const {
    return num_nodes_;
  }

 protected:
  topology* topology_;
  int num_nodes_;
  int num_switches_;

 private:
  static interconnect* static_interconnect_;

  interconnect(){}

#if !SSTMAC_INTEGRATED_SST_CORE
 public:
  typedef spkt_unordered_map<switch_id, network_switch*> switch_map;
  typedef spkt_unordered_map<switch_id, connectable*> internal_map;
  typedef spkt_unordered_map<node_id, connectable*> endpoint_map;
  typedef spkt_unordered_map<node_id, node*> node_map;
  typedef spkt_unordered_map<node_id, nic*> nic_map;

  std::string
  to_string() const {
    return "interconnect";
  }

  virtual ~interconnect();

  topology*
  topol() const {
    return topology_;
  }


  inline timestamp send_delay(int num_hops, int num_bytes) const {
    double bw_term = num_bytes / hop_bw_;
    timestamp delay = hop_latency_ * num_hops + timestamp(bw_term) + 2*injection_latency_;
    return delay;
  }

  /**
    Do not actually inject the message into the network
    or do any congestion modeling. Just immediately
    schedule the message at its destination after some
    computed delay.
    @param msg The message to send to the destination
  */
  void immediate_send(event_scheduler* src, message* msg, timestamp start) const;

  /**
   * @brief Return the node corresponding to given ID.
   *        No bounds checking is done for validity of ID.
   *        NULL is a valid return value for parallel simulation
   *        since it means node belongs to another process
   * @param nid The ID of the node object to get
   * @return The node object or NULL, if ID is not found
   */
  node*
  node_at(node_id nid) const {
    node_map::const_iterator it = nodes_.find(nid);
    if (it == nodes_.end()){
      return 0;
    } else {
      return it->second;
    }
  }

  const node_map&
  nodes() const {
    return nodes_;
  }

  void
  kill_node(node_id nid);

  void
  kill_node(node_id nid, timestamp t);

  void
  deadlock_check();

  void handle(event* ev);

  network_switch*
  switch_at(switch_id id) const;

  const switch_map&
  switches() const {
    return switches_;
  }

  int
  thread_for_switch(switch_id sid) const;

  timestamp
  hop_latency() const {
    return hop_latency_;
  }

  timestamp
  lookahead() const {
    return lookahead_;
  }

  bool
  local_speedy_node(node_id nid) const {
    return node_to_speedy_switch_[nid] == local_speedy_switch_;
  }

 private:
  switch_map switches_;
  //a set of switches that transfer messages quickly
  switch_map speedy_overlay_switches_;
  node_map nodes_;
  nic_map nics_;

  switch_id local_speedy_switch_;
  std::vector<switch_id> node_to_speedy_switch_;

  double hop_bw_;

  timestamp hop_latency_;

  timestamp injection_latency_;

  timestamp lookahead_;

  int num_speedy_switches_with_extra_node_;
  int num_nodes_per_speedy_switch_;

  partition* partition_;
  parallel_runtime* rt_;

  typedef spkt_unordered_map<netlink_id, netlink*> netlink_map;
  netlink_map netlinks_;

  typedef std::pair<timestamp, node_id> node_fail_event;
  std::list<node_fail_event> failures_to_schedule_;
#endif
};

DeclareFactory3InitParams(interconnect, event_manager*, partition*, parallel_runtime*);

}
} // end of namespace sstmac

#endif

