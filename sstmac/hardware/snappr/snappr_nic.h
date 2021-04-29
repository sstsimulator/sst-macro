/**
Copyright 2009-2021 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2021, NTESS

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

#ifndef SnapprNIC_h
#define SnapprNIC_h

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/memory/memory_model_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/snappr/snappr_switch.h>
#include <sstmac/hardware/common/recv_cq.h>


namespace sstmac {
namespace hw {

/**
 @class SnapprNIC
 Network interface compatible with snappr network model
 */
class SnapprNIC :
  public NIC
{
 public:
#if SSTMAC_INTEGRATED_SST_CORE
  SST_ELI_REGISTER_SUBCOMPONENT_DERIVED(
    SnapprNIC,
    "macro",
    "snappr_nic",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "A NIC implementing the snappr model",
    sstmac::hw::NIC)

  SST_ELI_DOCUMENT_SUBCOMPONENT_SLOTS(
      {"outport", "The injection output port for the NIC", "sstmac::SnapprOutport"},
  )
#else
  SST_ELI_REGISTER_DERIVED(
    NIC,
    SnapprNIC,
    "macro",
    "snappr",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "A NIC implementing the snappr model")
#endif

  SnapprNIC(uint32_t id, SST::Params& params, Node* parent);

  std::string toString() const override {
    return sprockit::sprintf("snappr nic(%d)", int(addr()));
  }

  void init(unsigned int phase) override;

  void setup() override;

  ~SnapprNIC() throw () override;

  void handlePayload(Event* ev);

  void handleCredit(Event* ev);

  void handleTailPacket(Timestamp done, SnapprPacket* pkt);

  void connectOutput(int src_outport, int dst_inport, EventLink::ptr&& link) override;

  void connectInput(int src_outport, int dst_inport, EventLink::ptr&& link) override;

  LinkHandler* creditHandler(int port) override;

  LinkHandler* payloadHandler(int port) override;

  struct InjectionQueue {
    SPKT_DECLARE_BASE(InjectionQueue)
    SPKT_DECLARE_CTOR(SST::Params&)

    virtual std::pair<uint64_t, NetworkMessage*> top() = 0;

    virtual void insert(uint64_t byte_offset, NetworkMessage* msg) = 0;

    virtual void pop() = 0;

    virtual void adjustTop(uint64_t byte_offset) = 0;

    virtual bool empty() const = 0;
  };

  void deadlockCheck() override;


 private:
  void doSend(NetworkMessage* payload) override;

  void cqHandle(SnapprPacket* pkt);

  void eject(SnapprPacket* pkt);

  void copyToNicBuffer();

  void injectPacket(uint32_t pkt_size, uint64_t byte_offset, NetworkMessage* payload);

  void handleMemoryResponse(MemoryModel::Request* req);

  EventLink::ptr credit_link_;

  uint32_t packet_size_;

  int switch_outport_;

  TimeDelta inj_byte_delay_;

  bool flow_control_;
  std::vector<SnapprOutPort*> outports_;
  InjectionQueue* inject_queue_;

  Timestamp ej_next_free_;
  RecvCQ cq_;

  uint64_t buffer_remaining_;

  MemoryModel* mem_model_;
  int mem_req_id_;
  bool ignore_memory_;

  int qos_levels_;
  bool scatter_qos_;
  int next_qos_;

  int rdma_get_req_qos_;

};

}
} // end of namespace sstmac


#endif // PiscesNIC_H
