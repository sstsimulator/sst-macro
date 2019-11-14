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

#ifndef sstmac_hardware_nic_PiscesNIC_H
#define sstmac_hardware_nic_PiscesNIC_H

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/common/recv_cq.h>

namespace sstmac {
namespace hw {

/**
 @class PiscesNIC
 Network interface compatible with sending pisces packets
 */
class PiscesNIC : public NIC
{
 public:
#if SSTMAC_INTEGRATED_SST_CORE
  SST_ELI_REGISTER_SUBCOMPONENT(
    PiscesNIC,
    "macro",
    "pisces_nic",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "implements a nic that models messages as a packet flow",
    "nic")
#else
  SST_ELI_REGISTER_DERIVED(
    NIC,
    PiscesNIC,
    "macro",
    "pisces",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "implements a nic that models messages as a packet flow")
#endif

  PiscesNIC(SST::Component* parent, SST::Params& params);

  std::string toString() const override {
    return sprockit::printf("packet flow nic(%d)", int(addr()));
  }

  void init(unsigned int phase) override;

  void setup() override;

  virtual ~PiscesNIC() throw ();

  virtual void connectOutput(int src_outport, int dst_inport, EventLink::ptr&& link) override;

  virtual void connectInput(int src_outport, int dst_inport, EventLink::ptr&& link) override;

  LinkHandler* creditHandler(int port) override;

  LinkHandler* payloadHandler(int port) override;

  void packetSent(Event* ev);

  void incomingPacket(Event* ev);

 private:
  void packetArrived(PiscesPacket* pkt);

  void doSend(NetworkMessage* payload) override;

  uint64_t inject(int vn, uint64_t offset, NetworkMessage* msg);

  struct Pending {
    uint64_t bytes_sent;
    uint64_t bytes_total;
    NetworkMessage* msg;

    Pending(uint64_t offset, uint64_t size, NetworkMessage* m) :
      bytes_sent(offset), bytes_total(size), msg(m)
    {
    }

  };

  std::vector<std::queue<Pending>> pending_inject_;

  PiscesBuffer* inj_buffer_;

  RecvCQ completion_queue_;

  EventLink::ptr credit_link_;
  EventLink::ptr self_mtl_link_;

  // TODOWARNING NodeId my_addr_;

  uint32_t packet_size_;
  uint32_t inj_credits_;

};


}
} // end of namespace sstmac


#endif // PiscesNIC_H
