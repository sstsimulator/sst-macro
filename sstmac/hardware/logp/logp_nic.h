/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
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
  FactoryRegister("logP | simple | LogP | logp", nic, logp_nic,
              "implements a nic that models messages via a simple latency/bandwidth injection delay")
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