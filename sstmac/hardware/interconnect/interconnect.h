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

#include <sstmac/backends/common/sim_partition_fwd.h>

#include <sprockit/debug.h>
#include <sprockit/factories/factory.h>
#include <sprockit/unordered.h>

#include <set>

DeclareDebugSlot(interconnect)

#define interconn_debug(...) \
  debug_printf(sprockit::dbg::interconnect, __VA_ARGS__)

#if SSTMAC_INTEGRATED_SST_CORE
#define SSTMAC_SET_TOPOLOGY(obj, top) //no op
#define STATIC_INIT_TOPOLOGY(params) \
{ \
  sstmac::hw::topology* top = sstmac::hw::topology::static_topology(params); \
  set_topology(top); \
} \

#define STATIC_INIT_INTERCONNECT(params) \
{ \
  sstmac::hw::interconnect* top = sstmac::hw::interconnect::static_interconnect(params); \
  set_interconnect(top); \
}

#else
#define SSTMAC_SET_TOPOLOGY(obj, top) obj->set_topology(top);
#define STATIC_INIT_TOPOLOGY(params) //no op
#define STATIC_INIT_INTERCONNECT(params)
#endif

namespace sstmac {
namespace hw {

/**
 * Base class for network congestion models.
 */
class interconnect :
  public sprockit::factory_type
{

 public:
  typedef spkt_unordered_map<switch_id, connectable*> internal_map;
  typedef spkt_unordered_map<node_id, connectable*> endpoint_map;


  typedef std::set<node_id> node_set;

  virtual std::string
  to_string() const {
    return "interconnect";
  }

  virtual ~interconnect();

  virtual int
  num_nodes() const;

  virtual topology*
  topol() const {
    return topology_;
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

#if !SSTMAC_INTEGRATED_SST_CORE
  /**
    Do not actually inject the message into the network
    or do any congestion modeling. Just immediately
    schedule the message at its destination after some
    computed delay.
    @param msg The message to send to the destination
  */
  virtual void
  immediate_send(event_scheduler* src, sst_message* msg, timestamp start) const = 0;

  virtual void
  set_event_manager(event_manager* mgr){};
#endif

  node_set& allocated() {
    return allocated_;
  }

  node_set& available() {
    return available_;
  }

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

  virtual void
  kill_node(node_id nid) = 0;

  virtual void
  kill_node(node_id nid, timestamp t) = 0;

  virtual void
  init_param1(partition* part) = 0;

  virtual void
  init_param2(parallel_runtime* rt) = 0;

  static interconnect*
  static_interconnect(sprockit::sim_parameters* params);

  virtual void
  deadlock_check(){}

 protected:
  interconnect();

 protected:
  /**
    JJW 12/05/2013
    The interconnect should really keep track of allocated and available nodes.
    This is moved here from allocation strategy.
    The allocation strategy still does the actual work, but the interconnect stores the arrays
  */
  node_set allocated_;
  node_set available_;

  topology* topology_;

  /// The node objects.
  /// We would make this a vector, but because of parallel
  /// It can happen that a weird subset of nodes is created
  typedef spkt_unordered_map<node_id, node*> node_map;
  node_map nodes_;

  typedef spkt_unordered_map<node_id, nic*> nic_map;
  nic_map nics_;

 private:
  static interconnect* static_interconnect_;

  void set_topology(topology* params);

};

#if SSTMAC_INTEGRATED_SST_CORE
class sst_interconnect : public interconnect
{

 public:
  event_loc_id
  event_location() const {
    return event_loc_id::null;
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  kill_node(node_id nid);

  virtual void
  kill_node(node_id nid, timestamp t);

  void
  init_param1(partition* part){};

  void
  init_param2(parallel_runtime* rt){};
};
typedef sst_interconnect interconnect_base;
#else
class macro_interconnect : public interconnect
{
 public:
  virtual ~macro_interconnect();

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

  virtual void
  set_event_manager(event_manager* m);

  void
  set_node_event_manager(node* the_node, event_manager* m);

  void
  set_event_manager_common(event_manager* m);

  void
  kill_node(node_id nid);

  void
  kill_node(node_id nid, timestamp t);

  void
  init_param1(partition* part) {
    partition_ = part;
  }

  void
  init_param2(parallel_runtime* rt) {
    rt_ = rt;
  }

  virtual void
  handle(sst_message* msg);

 protected:
  macro_interconnect();

 protected:
  partition* partition_;
  parallel_runtime* rt_;

  typedef spkt_unordered_map<netlink_id, netlink*> netlink_map;
  netlink_map netlinks_;

  typedef std::pair<timestamp, node_id> fail_event;
  std::list<fail_event> failures_to_schedule_;
};
typedef macro_interconnect interconnect_base;
#endif

DeclareFactory2InitParams(interconnect, partition*, parallel_runtime*);

}
} // end of namespace sstmac

#endif

