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

#ifndef SIMPLE_NIC_H
#define SIMPLE_NIC_H

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/analytic/simple/simple_interconnect.h>

#if !SSTMAC_INTEGRATED_SST_CORE

namespace sstmac {
namespace hw {

/**
 * A networkinterface is a delegate between a node and a server module.
 * This object helps ornament network operations with information about
 * the process (ppid) involved.
 */
class simple_nic :
  public nic
{
 public:
  simple_nic(sprockit::factory_type* interconn) :
    nic(interconn), next_free_(0) {}

  /// Goodbye.
  virtual ~simple_nic() {}

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

  virtual void
  finalize_init();

  void handle(event *ev);

  virtual void
  connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod,
    config* cfg);

  virtual std::string
  to_string() const {
    return "simple nic";
  }

  timestamp
  injection_latency() const {
    return inj_lat_;
  }

  double
  injection_bandwidth() const {
    return 1.0/inj_bw_inverse_;
  }

 protected:
  /**
    Start the message sending and inject it into the network
    @param payload The network message to send
  */
  virtual void
  do_send(network_message* msg);

 protected:
  double inj_bw_inverse_;

  timestamp inj_lat_;

  timestamp next_free_;

 private:
  void send_to_injector(timestamp arrival, network_message* netmsg);
  void send_to_node(timestamp arrival, network_message* netmsg);

};

}
} // end of namespace sstmac.

#endif

#endif // SIMPLE_NIC_H

