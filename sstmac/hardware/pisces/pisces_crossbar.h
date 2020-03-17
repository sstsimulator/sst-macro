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

#ifndef PACKETFLOW_CROSSBAR_H
#define PACKETFLOW_CROSSBAR_H

#include <sstmac/hardware/pisces/pisces_sender.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/keyword_registration.h>

#include <memory>

namespace sstmac {
namespace hw {

class PiscesNtoMQueue :
  public PiscesSender
{
 public:
  ~PiscesNtoMQueue() override;

  PiscesNtoMQueue(const std::string& selfname, uint32_t id,
                  const std::string& arb, double bw,
                  SST::Component* parent,
                  int num_in_ports, int num_out_ports, int num_vc,
                  bool update_vc);

  void handlePayload(Event* ev) override;

  void handleCredit(Event* ev) override;

  LinkHandler* creditHandler();

  LinkHandler* payloadHandler();

  void setInput(int my_inport, int src_outport, EventLink::ptr&& link) override;

  void setOutput(int my_outport, int dst_inport, EventLink::ptr&& link, int credits) override;

  inline int slot(int port, int vc) const {
    return port * num_vc_ + vc;
  }

 protected:
  PiscesBandwidthArbitrator* arb_;

  std::vector<Input> inputs_;
  std::vector<Output> outputs_;
  //indexed by slot number = (port,vc)
  std::vector<int> credits_;
  //indexed by slot number = (port,vc)
  std::vector<PayloadQueue> queues_;
#if SSTMAC_SANITY_CHECK
  std::vector<int> initial_credits_;
#endif

  int num_vc_;

  std::map<int, std::set<int> > deadlocked_channels_;

  std::map<int, std::map<int, std::list<PiscesPacket*> > > blocked_messages_;

 protected:
  void sendPayload(PiscesPacket* pkt);

 private:
  inline int& credit(int port, int vc){
    return credits_[slot(port, vc)];
  }

  void resizeOutports(int num_ports);

  inline PayloadQueue& queue(int port, int vc){
    return queues_[slot(port, vc)];
  }

  std::string inputName(PiscesPacket* pkt);

  std::string outputName(PiscesPacket* pkt);

  EventLink* outputLink(PiscesPacket* pkt);

};

class PiscesDemuxer :
  public PiscesNtoMQueue
{
 public:
  PiscesDemuxer(const std::string& selfname, uint32_t id,
                const std::string& arb, double bw,
                SST::Component* parent,
                int num_out_ports, int num_vc,
                bool update_vc);

  std::string piscesName() const override {
    return "demuxer";
  }

};


class PiscesMuxer :
  public PiscesNtoMQueue
{
 public:
  PiscesMuxer(const std::string& selfname, uint32_t id,
              const std::string& arb, double bw,
              SST::Component* parent,
              int num_in_ports, int num_vc,
              bool update_vc);

  std::string piscesName() const override {
    return "muxer";
  }
};

class PiscesCrossbar :
  public PiscesNtoMQueue
{
 public:
  PiscesCrossbar(const std::string& name, uint32_t id,
                 const std::string& arb, double bw,
                 SST::Component* parent,
                 int num_in_ports, int num_out_ports, int num_vc,
                 bool update_vc);

  std::string piscesName() const override {
    return "crossbar";
  }
};


}
}

#endif // PACKETFLOW_CROSSBAR_H
