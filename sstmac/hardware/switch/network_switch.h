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

/**
 * @brief The network_switch class
 * A class encapsulating a network switch that packets must traverse on the network.
 * The network switch performs both routing computations and congestion modeling.
 */
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

  topology*
  topol() const {
    return top_;
  }

  switch_id
  addr() const {
    return my_addr_;
  }

  /**
   * @brief rter
   * @return The router used for performing routing computations
   */
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

  /**
   * @brief queue_length
   * Compute the number of packets waiting on the switch. The queue length
   * is a multiple of the ``system'' packet size, which can be different from the
   * packet size used by SST/macro congestion models. For example,
   * the packet size of the system beings simulated might be 100B, but SST/macro
   * might be doing congestion computations on units of 1024B.
   * @param port The port to check the queue length of
   * @return The queue length as an integer number of packets waiting
   */
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

  /**
   * @brief lookahead For parallel simulations, returns the maximum PDES lookahead possible
   * between two network switches. This is usually (but not always) the sames as the hop latency
   * @return The PDES lookahead
   */
  virtual timestamp
  lookahead() const = 0;

  /**
   @return The bandwidth observed hopping from switch to switch (with zero congestion)
  */
  virtual double
  hop_bandwidth() const = 0;

  /**
   * @brief connect
   * @param src_outport The port that packets will exit on the source
   * @param dst_inport  The port that packets will enter on the destination
   * @param ty          The type of connection (usually just output or input)
   *                    An output connection is a data link that carries payload
   *                    from src to dst. An input connection is a control link that
   *                    usually just carries credit/arbitration info from dst to src.
   * @param mod         The dst (if output link) or src (if input link)
   * @param cfg         An special configure options for the link
   */
  void
  connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod,
    config* cfg);

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

  virtual void
  connect_output(
    int src_outport,
    int dst_inport,
    connectable* mod,
    config* cfg) = 0;

  virtual void
  connect_input(
    int src_outport,
    int dst_inport,
    connectable* mod,
    config* cfg) = 0;

 protected:
  switch_id my_addr_;
  router* router_;
  topology* top_;

};

#if !SSTMAC_INTEGRATED_SST_CORE
DeclareFactory(network_switch);
#endif


}
}

#endif

