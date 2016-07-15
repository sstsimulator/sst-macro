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

#ifndef SSTMAC_HARDWARE_NETWORK_SWITCHES_SWITCHINTERCONNECT_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_SWITCHES_SWITCHINTERCONNECT_H_INCLUDED

#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/interconnect/switch_interconnect_fwd.h>
#include <sstmac/hardware/switch/network_switch_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sprockit/unordered.h>

namespace sstmac {
namespace hw {

class switch_interconnect_base : public interconnect_base
{
 protected:
  inline timestamp send_delay(int num_hops, int num_bytes) const {
    double bw_term = num_bytes / hop_bw_;
    timestamp delay = hop_latency_ * num_hops + timestamp(bw_term) + 2*injection_latency_;
    return delay;
  }

 protected:
  double hop_bw_;

  timestamp hop_latency_;

  timestamp injection_latency_;

  timestamp lookahead_;

};

#if SSTMAC_INTEGRATED_SST_CORE
class sst_switch_interconnect :
  public switch_interconnect_base
{
 public:
  virtual void
  init_factory_params(sprockit::sim_parameters* params);
  event_loc_id
  event_location() const {
    return event_loc_id::null;
  }
};
#else
class macro_switch_interconnect :
  public switch_interconnect_base
{
 public:
  typedef spkt_unordered_map<switch_id, network_switch*> switch_map;

 public:
  macro_switch_interconnect();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual std::string
  to_string() const {
    return "switchinterconnect";
  }

  virtual ~macro_switch_interconnect();

  static network_switch*
  template_switch(sprockit::sim_parameters* params,
                  topology* topol,
                  switch_id sid = switch_id());

  virtual void
  set_event_manager(event_manager* m);

  network_switch*
  switch_at(switch_id id) const
  {
    switch_map::const_iterator it = switches_.find(id);
    if (it == switches_.end()){
      return 0;
    }
    return it->second;
  }

  const switch_map&
  switches() const {
    return switches_;
  }

  virtual void
  immediate_send(event_scheduler* src, message* msg, timestamp start) const;

  void
  write_graph_file(const std::string& graph_file);

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

  event_loc_id
  event_location() const {
    return event_loc_id::null;
  }

  void deadlock_check();

 protected:
  void
  set_switch_event_manager(
    int thread_id, switch_id sid,
    event_manager* m);

 protected:
  switch_map switches_;

};

#endif

}
}

#endif

