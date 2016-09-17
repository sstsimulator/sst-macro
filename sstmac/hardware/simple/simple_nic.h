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
  simple_nic(sprockit::sim_parameters* params, node* parent);

  /// Goodbye.
  virtual ~simple_nic() {}

  void handle(event *ev) override;

  virtual void
  connect_output(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    connectable* mod) override;

  virtual void
  connect_input(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    connectable* mod) override;

  virtual std::string
  to_string() const override {
    return "simple nic";
  }

 protected:
  /**
    Start the message sending and inject it into the network
    @param payload The network message to send
  */
  virtual void
  do_send(network_message* msg) override;

 protected:
  double inj_bw_inverse_;

  timestamp inj_lat_;

  timestamp next_free_;

};

}
} // end of namespace sstmac.

#endif

#endif // SIMPLE_NIC_H

