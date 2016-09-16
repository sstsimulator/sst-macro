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

#ifndef SSTMAC_HARDWARE_NETWORK_SWITCHES_DIST_DUMMYSWITCH_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_SWITCHES_DIST_DUMMYSWITCH_H_INCLUDED

#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/router/routable.h>

#if !SSTMAC_INTEGRATED_SST_CORE

namespace sstmac {
namespace hw {

/**
 * @brief The dist_dummy_switch class
 * Encapsulates a switch that is a placeholder used in parallel simulation.
 * A link between a real switch and a dummy switch represents a link between
 * switches on different MPI ranks.
 */
class dist_dummy_switch : public network_switch
{
 public:
  dist_dummy_switch(sprockit::sim_parameters* params, uint64_t sid, event_manager* mgr)
    : network_switch(params, sid, mgr)
  {
    init_loc_id(event_loc_id(my_addr_));
  }

  std::string
  to_string() const override ;

  bool
  ipc_handler() const override {
    return true;
  }

  virtual
  ~dist_dummy_switch() {
  }

  virtual void
  handle(event* ev) override;

  virtual void
  connect_input(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    connectable* comp) override;

  virtual void
  connect_output(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    connectable* comp) override;

  std::vector<switch_id>
  connected_switches() const override {
    spkt_throw(sprockit::unimplemented_error,
              "dist_dummyswitch::connected_switches: should not be called on dummy switch");
  }

  int
  queue_length(int port) const override {
    return 0;
  }


};

}
}

#endif

#endif

