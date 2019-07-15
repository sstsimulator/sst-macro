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

  struct priority_less {
    bool operator()(SnapprPacket* l, SnapprPacket* r) const {
      if (l->priority() == r->priority()){
        return l->seqnum() > r->seqnum();
      } else {
        return l->priority() < r->priority();
      }
    }
  };

  struct OutputQueue {
    bool addCredits(uint32_t bytes){
      uint32_t holSize = packets_.empty() ? 0 : packets_.front()->byteLength();
      bool stalled = holSize > credits_;
      credits_ += bytes;
      //pass back if were stalled but aren't anymore
      return stalled && holSize <= credits_;
    }

    bool push(SnapprPacket* pkt){
      bool newly_active = packets_.empty() && pkt->byteLength() <= credits_;
      packets_.push(pkt);
      return newly_active;
    }

    size_t size() const {
      return packets_.size();
    }

    bool spaceToSend() const {
      SnapprPacket* pkt = packets_.front();
      return pkt->byteLength() <= credits_;
    }

    SnapprPacket* pop(bool& stalled) {
      SnapprPacket* pkt = packets_.front();
      packets_.pop();
      credits_ -= pkt->byteLength();
      stalled = packets_.empty() || packets_.front()->byteLength() > credits_;
      return pkt;
    }

    OutputQueue(uint32_t credits) : credits_(credits){}

    void scaleBuffers(double scale){
      credits_ *= scale;
    }

   private:
    std::queue<SnapprPacket*> packets_;
    uint32_t credits_;
  };

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

    bool readyVirtualLanes() const {
      return ready_lanes_.size();
    }

    int queueLength(int vl) const {
      return queues_[vl].size();
    }

    int queueLength() const {
      return total_packets_;
    }

    bool empty() const {
      return total_packets_ == 0;
    }

    void addCredits(int vl, uint32_t credits){
      bool lane_activated = queues_[vl].addCredits(credits);
      if (lane_activated){
        ready_lanes_.push(vl);
      }
    }

    void scaleBuffers(double factor){
      for (auto& queue : queues_){
        queue.scaleBuffers(factor);
      }
    }

    SnapprPacket* popReady(){
      int vl = ready_lanes_.top();
      bool stalled;
      SnapprPacket* pkt = queues_[vl].pop(stalled);
      if (stalled){
        ready_lanes_.pop();
      }
      --total_packets_;
      return pkt;
    }

    void queue(SnapprPacket* pkt){
      OutputQueue& q = queues_[pkt->virtualLane()];
      bool queue_activated = q.push(pkt);
      if (queue_activated){
        ready_lanes_.push(pkt->virtualLane());
      }
      total_packets_++;
    }

    void setVirtualLanes(int num_vl, uint32_t total_credits){
      uint32_t credits_per_vl = total_credits / num_vl;
      queues_.resize(num_vl, OutputQueue(credits_per_vl));
    }

    EventLink::ptr link;
    OutPort() : link(nullptr), arbitration_scheduled(false), total_packets_(0) {}

   private:
    std::priority_queue<int, std::vector<int>> ready_lanes_;
    std::vector<OutputQueue> queues_;
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
