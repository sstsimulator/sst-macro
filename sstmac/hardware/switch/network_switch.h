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

#ifndef SSTMAC_HARDWARE_NETWORK_SWITCHES_NETWORKSWITCH_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_SWITCHES_NETWORKSWITCH_H_INCLUDED



#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>

#include <sstmac/hardware/common/connection.h>
#include <sstmac/hardware/common/clonable.h>
#include <sstmac/hardware/router/router_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sprockit/sim_parameters_fwd.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/integrated_component.h>
#endif

#include <vector>

DeclareDebugSlot(network_switch)

namespace sstmac {
namespace hw {

class network_switch :
  public connectable_component,
  public sprockit::factory_type
{
 public:
  std::string
  to_string() const {
    return "network switch";
  }

#if SSTMAC_INTEGRATED_SST_CORE
  network_switch(
      SST::ComponentId_t id,
      SST::Params& params
  );

  virtual void
  init(unsigned int phase);

  virtual void
  setup();
#endif

  virtual ~network_switch();

  static inline int
  packet_length_bytes() {
    return packet_length_bytes_;
  }

  topology*
  topol() const {
    return top_;
  }

  switch_id
  addr() const {
    return my_addr_;
  }

  router*
  rter() const {
    return router_;
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  initialize() {
    //nothing to do by default
  }

  virtual std::vector<switch_id>
  connected_switches() const = 0;

  virtual void
  set_topology(topology* top);

  virtual int
  queue_length(int port) const = 0;

  /**
    @param addr The destination node addr to eject to
    @return The ejection port the node is connected on
  */
  int
  eject_port(node_id addr);

  /**
    @param addr The source node addr to ack to
    @return The injection port the node is connected on
  */
  int
  inject_port(node_id addr);

  /**
   @return The total hop latency to transit from input of one switch to the next (with zero congestion)
  */
  virtual timestamp
  hop_latency() const = 0;

  virtual timestamp
  lookahead() const = 0;

  /**
   @return The bandwidth observed hopping from switch to switch (with zero congestion)
  */
  virtual double
  hop_bandwidth() const = 0;

  void
  connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod);

  virtual void
  set_event_manager(event_manager* m);
#if !SSTMAC_INTEGRATED_SST_CORE
  protected:
  network_switch();
#endif
 protected:
  virtual void
  connect_injector(int src_outport, int dst_inport, event_handler* nic) = 0;

  virtual void
  connect_ejector(int src_outport, int dst_inport, event_handler* nic) = 0;

 protected:
  switch_id my_addr_;
  router* router_;
  topology* top_;

  static int packet_length_bytes_;

};

#if !SSTMAC_INTEGRATED_SST_CORE
DeclareFactory(network_switch);
#endif


}
}

#endif

