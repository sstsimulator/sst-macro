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

#ifndef SnapprSwitch_h
#define SnapprSwitch_h

#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/snappr/snappr.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/stats/ftq_fwd.h>
#include <queue>

namespace sstmac {
namespace hw {

/**
 @class SnapprSwitch
 A switch in the network that arbitrates/routes
 to the next link in the network
 */
class SnapprSwitch :
  public NetworkSwitch
{

 public:
  SST_ELI_REGISTER_DERIVED_COMPONENT(
    NetworkSwitch,
    SnapprSwitch ,
    "macro",
    "snappr_switch",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "A network switch implementing the snappr model",
    COMPONENT_CATEGORY_NETWORK)

  SST_ELI_DOCUMENT_PORTS(SSTMAC_VALID_PORTS)

  SST_ELI_DOCUMENT_STATISTICS(
    { "traffic_intensity",    "Count the traffic on a port", "unit of traffic", 1},
    {"xmit_wait", "congestion statistic", "cycles", 1}, // Name, Desc, Units, Enable Level
  )

  SnapprSwitch(uint32_t id, SST::Params& params);

  virtual ~SnapprSwitch();

  int queueLength(int port, int vc) const override;

  Router* router() const override {
    return router_;
  }

  void connectOutput(int src_outport, int dst_inport, EventLink::ptr&& link) override;

  void connectInput(int src_outport, int dst_inport, EventLink::ptr&& link) override;

  LinkHandler* creditHandler(int port) override;

  LinkHandler* payloadHandler(int port) override;

  std::string toString() const override;

  void deadlockCheck() override;

 private:
  friend class InPort;
  friend class OutPort;

  void handleCredit(SnapprCredit* ev, int port);

  void handlePayload(SnapprPacket* ev, int port);

  struct OutPort {
    int number;
    int dst_port;
    bool arbitration_scheduled;
    Timestamp next_free;
    Timestamp stall_start;
    Timestamp send_start;
    Timestamp last_queue_depth_collection;
    TimeDelta byte_delay;
    SST::Statistics::Statistic<uint64_t>* xmit_stall;
    SST::Statistics::Statistic<uint64_t>* xmit_active;
    SST::Statistics::Statistic<uint64_t>* xmit_idle;
    SST::Statistics::Statistic<uint64_t>* bytes_sent;
    sstmac::FTQCalendar* state_ftq;
    sstmac::FTQCalendar* queue_depth_ftq;
    SnapprSwitch* parent;
    std::string toString() const;

    void handle(Event* ev);

    int queueLength(int vl) const {
      return arb_->queueLength(vl);
    }

    int queueLength() const {
      return total_packets_;
    }

    bool ready() const {
      return !arb_->empty();
    }

    bool empty() const {
      return total_packets_ == 0;
    }

    void addCredits(int vl, uint32_t credits){
      arb_->addCredits(vl, credits);
    }

    void scaleBuffers(double factor){
      arb_->scaleCredits(factor);
    }

    SnapprPacket* popReady(){
      --total_packets_;
      return arb_->pop(parent->now().time.ticks());
    }

    void queue(SnapprPacket* pkt){
      arb_->insert(parent->now().time.ticks(), pkt);
      total_packets_++;
    }

    void setVirtualLanes(int num_vl, uint32_t total_credits){
      //uint32_t credits_per_vl = total_credits / num_vl;
      arb_->setVirtualLanes(num_vl, total_credits);
    }

    EventLink::ptr link;
    OutPort(const std::string& arb);
    //: link(nullptr), arbitration_scheduled(false), total_packets_(0) {}

   private:
    SnapprPortArbitrator* arb_;
    int total_packets_;

  };
  std::vector<OutPort> outports_;

  struct InPort {
    int number;
    int src_outport;
    EventLink::ptr link;
    SnapprSwitch* parent;
    void handle(Event* ev);

    std::string toString() const;
  };

  std::vector<InPort> inports_;

  Router* router_;

  bool congestion_;

  bool send_credits_;

  double link_bw_;

  int qos_levels_;

  int num_vc_;
  int num_vl_;

  std::vector<int> ftq_idle_states_;
  std::vector<int> ftq_active_states_;
  std::vector<int> ftq_stalled_states_;

 private:
  void send(OutPort& p, SnapprPacket* pkt, Timestamp now);

  void tryToSendPacket(SnapprPacket* pkt);

  void arbitrate(int port);

  void requestArbitration(OutPort& p);

  void scheduleArbitration(OutPort& p);

  void logQueueDepth(OutPort& p);

};

}
}

#endif
