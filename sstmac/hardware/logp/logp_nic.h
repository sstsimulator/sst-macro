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

namespace sstmac {
namespace hw {

/**
 * @brief Implements a NIC that does very basic congestion modeling
 *        using the LogGP model.  See "LogGP in Theory and Practice"
 *        by Hoefler and Schneider.
 */
class logp_nic :
  public nic
{
 public:
  logp_nic(sprockit::sim_parameters* params, node* parent);

  /// Goodbye.
  virtual ~logp_nic();

  void handle(event *ev);

  virtual void
  connect_output(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_handler* handler) override;

  virtual void
  connect_input(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_handler* handler) override;

  virtual std::string
  to_string() const override {
    return "simple nic";
  }

  link_handler*
  credit_handler(int port) const override {
    return nullptr; //should never handle acks
  }

  link_handler*
  payload_handler(int port) const override;

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

  event_handler* ack_handler_;

#if !SSTMAC_INTEGRATED_SST_CORE
  link_handler* payload_handler_;
#endif

};

}
} // end of namespace sstmac.

#endif // SIMPLE_NIC_H

