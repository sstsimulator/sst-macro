/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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
class LogPNIC :
  public NIC
{
 public:
#if SSTMAC_INTEGRATED_SST_CORE
  SST_ELI_REGISTER_SUBCOMPONENT_DERIVED(
    LogPNIC,
    "macro",
    "logp_nic",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "implements a nic that models messages via a simple latency/bandwidth injection delay",
    sstmac::hw::NIC)
#else
  SST_ELI_REGISTER_DERIVED(
    NIC,
    LogPNIC,
    "macro",
    "logp",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "implements a nic that models messages via a simple latency/bandwidth injection delay")
#endif

  LogPNIC(uint32_t id, SST::Params& params, Node* parent);

  virtual ~LogPNIC();

  void handle(Event *ev);

  virtual void connectOutput(int src_outport, int dst_inport, EventLink::ptr&& link) override;

  virtual void connectInput(int src_outport, int dst_inport, EventLink::ptr&& link) override;

  void mtlHandle(Event* ev) override;

  void dropEvent(Event*){}

  virtual std::string toString() const override {
    return "simple nic";
  }

  LinkHandler* creditHandler(int  /*port*/) override {
    return newLinkHandler(this, &LogPNIC::dropEvent);
  }

  LinkHandler* payloadHandler(int port) override;

 protected:
  /**
    Start the message sending and inject it into the network
    @param payload The network message to send
  */
  virtual void doSend(NetworkMessage* msg) override;

 protected:
  TimeDelta inj_byte_delay_;

  TimeDelta inj_lat_;

  Timestamp next_out_free_;

  Timestamp next_in_free_;

};

}
} // end of namespace sstmac.

#endif // SIMPLE_NIC_H
