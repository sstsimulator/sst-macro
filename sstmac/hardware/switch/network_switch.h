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
#define switch_debug(...) \
  debug_printf(sprockit::dbg::network_switch, "Switch %d: %s", \
    int(addr()), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

/**
 * @brief The network_switch class
 * A class encapsulating a network switch that packets must traverse on the network.
 * The network switch performs both routing computations and congestion modeling.
 */
class network_switch :
  public connectable_component
{
 public:
  virtual void
  init(unsigned int phase);

  virtual ~network_switch();

  switch_id
  addr() const {
    return my_addr_;
  }

  virtual void
  compatibility_check() const {
    //by default, nothing
  }

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


 protected:
  network_switch(
    sprockit::sim_parameters* params,
    uint64_t id,
    event_manager* mgr,
    device_id::type_t ty = device_id::router);

  switch_id my_addr_;
  topology* top_;

};


DeclareFactory(network_switch,uint64_t,event_manager*);


}
}

#endif

