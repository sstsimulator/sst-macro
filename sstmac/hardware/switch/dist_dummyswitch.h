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

class dist_dummy_switch : public network_switch
{
 public:
  dist_dummy_switch(switch_id sid)
  {
    my_addr_ = sid;
    init_loc_id(event_loc_id(sid));
  }

  std::string
  to_string() const;

  bool
  ipc_handler() const {
    return true;
  }

  virtual
  ~dist_dummy_switch() {
  }

  virtual void
  handle(event* ev);

  virtual void
  connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod) 
  { //no op 
  }

  virtual void
  connect_input(
    int src_outport,
    int dst_inport,
    connectable* comp,
    config* cfg);

  virtual void
  connect_output(
    int src_outport,
    int dst_inport,
    connectable* comp,
    config* cfg);

  std::vector<switch_id>
  connected_switches() const {
    spkt_throw(sprockit::unimplemented_error,
              "dist_dummyswitch::connected_switches: should not be called on dummy switch");
  }

  double
  hop_bandwidth() const;

  timestamp
  hop_latency() const;

  timestamp
  lookahead() const;

  int
  queue_length(int port) const {
    return 0;
  }

 protected:
  virtual void
  connect_ejector(int src_outport, int dst_inport, event_handler* nic);

  virtual void
  connect_injector(int src_outport, int dst_inport, event_handler* nic);


};

}
}

#endif

#endif

