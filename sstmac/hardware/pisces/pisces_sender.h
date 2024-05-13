/**
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

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

#ifndef PACKETFLOW_CREDITOR_H
#define PACKETFLOW_CREDITOR_H

#include <sprockit/util.h>
#include <sstmac/common/stats/stat_spyplot_fwd.h>
#include <sstmac/hardware/pisces/pisces.h>
#include <sstmac/hardware/pisces/pisces_arbitrator.h>
#include <sstmac/common/event_scheduler.h>

#define pisces_debug(...) \
  debug_printf(sprockit::dbg::pisces, __VA_ARGS__)

namespace sstmac {
namespace hw {

struct PayloadQueue {

  std::list<PiscesPacket*> queue;

  typedef std::list<PiscesPacket*>::iterator iterator;

  PiscesPacket* pop(int num_credits);

  PiscesPacket* front(){
    if (queue.empty()){
      return nullptr;
    }
    return queue.front();
  }


  size_t size() const {
    return queue.size();
  }

  void push_back(PiscesPacket* payload){
    queue.push_back(payload);
  }

};

class PiscesSender : public SubComponent
{
 public:
  struct Input {
    int port_to_credit;
    EventLink::ptr link;
    Input() : link(nullptr){}
  };

  struct Output {
    int arrival_port;
    EventLink::ptr link;
    Output() : link(nullptr){}
  };

  virtual ~PiscesSender() {}

  virtual void setInput(int my_inport, int dst_outport, EventLink::ptr&& link) = 0;

  virtual void setOutput(int my_outport, int dst_inport, EventLink::ptr&& link, int credits) = 0;

  virtual void handlePayload(Event* ev) = 0;

  virtual void handleCredit(Event* ev) = 0;

  virtual std::string piscesName() const = 0;

  std::string toString() const override;// override;

 protected:
  PiscesSender(const std::string& selfname, uint32_t id,
               SST::Component* parent, bool update_vc);

  void sendCredit(Input& inp, PiscesPacket* payload,
          Timestamp packet_tail_leaves);

  /**
   * @brief send Invoked to send/bandwidth arbitrator a packet.
   * Only called when there are enough credits to hold packet on other side.
   * @param arb
   * @param pkt
   * @param to_credit
   * @param to_send
   * @return
   */
  Timestamp send(PiscesBandwidthArbitrator* arb,
       PiscesPacket* pkt, Input& to_credit, Output& to_send);

 protected:
  TimeDelta send_lat_;

  TimeDelta credit_lat_;

  bool update_vc_;

};

}
}

#endif // PACKETFLOW_CREDITOR_H
