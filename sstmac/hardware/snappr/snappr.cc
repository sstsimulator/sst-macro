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

#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif

#include <inttypes.h>
#include <queue>

#include <sstmac/hardware/snappr/snappr.h>

RegisterDebugSlot(snappr, "print all the details of the snappr model")

#define port_debug(...) \
  debug_printf(sprockit::dbg::snappr, __VA_ARGS__)

namespace sstmac {
namespace hw {

SnapprPacket::SnapprPacket(
  Flow* msg,
  uint32_t num_bytes,
  bool is_tail,
  uint64_t flow_id,
  uint64_t offset,
  NodeId toaddr,
  NodeId fromaddr,
  int qos) :
  offset_(offset),
  priority_(0),
  inport_(-1),
  qos_(qos),
  Packet(msg, num_bytes, flow_id, is_tail, fromaddr, toaddr)
{
}

std::string
SnapprPacket::toString() const
{
  return sprockit::printf("pkt bytes=%" PRIu32 " flow=%" PRIu64 ": %s",
                          numBytes(), flowId(), flow()
                          ? flow()->toString().c_str()
                          : "no payload");

}

void
SnapprPacket::serialize_order(serializer& ser)
{
  //routable::serialize_order(ser);
  Packet::serialize_order(ser);
  ser & arrival_;
  ser & time_to_send_;
  ser & priority_;
  ser & inport_;
  ser & qos_;
  ser & vl_;
  ser & input_vl_;
}

std::string
SnapprCredit::toString() const {
  return sprockit::printf("credit bytes=%" PRIu32 " port=%d", num_bytes_, port_);
}

void
SnapprCredit::serialize_order(serializer &ser)
{
  Event::serialize_order(ser);
  ser & port_;
  ser & num_bytes_;
  ser & vl_;
}

struct FifoPortArbitrator : public SnapprPortArbitrator
{
  struct VirtualLane {
    uint32_t credits;
    int occupancy;
    std::queue<SnapprPacket*> pending;
    VirtualLane() : occupancy(0){}
  };

 public:
  SPKT_REGISTER_DERIVED(
    SnapprPortArbitrator,
    FifoPortArbitrator,
    "macro",
    "fifo",
    "implements a FIFO strategy for queuing packets")

  void insert(uint64_t cycle, SnapprPacket *pkt) override {
    VirtualLane& vl = vls_[pkt->virtualLane()];
    vl.occupancy += 1;
    if (vl.credits >= pkt->numBytes()){
      port_debug("FIFO %p VL %d queueing with %u credits - packet %s",
                 this, pkt->virtualLane(), vl.credits, pkt->toString().c_str());
      vl.credits -= pkt->numBytes();
      port_queue_.push(pkt);
    } else {
      vl.pending.push(pkt);
      port_debug("FIFO %p VL %d stalling with %u credits - packet %s",
                 this, pkt->virtualLane(), vl.credits, pkt->toString().c_str());
    }
  }

  int queueLength(int vl) const override {
    auto& v = vls_[vl];
    return v.pending.size();
  }

  void scaleCredits(double factor) override {
    for (VirtualLane& vl : vls_){
      vl.credits *= factor;
    }
  }

  void setVirtualLanes(int num_vl, uint32_t total_credits) override {
    uint32_t credits_per_vl = total_credits / num_vl;
    vls_.resize(num_vl);
    for (VirtualLane& vl : vls_){
      vl.credits = credits_per_vl;
    }
  }

  void addCredits(int vl, uint32_t credits) override {
    VirtualLane& v = vls_[vl];
    v.credits += credits;
    port_debug("FIFO %p VL %d adding credits up to %u",
               this, vl, v.credits);
    while (!v.pending.empty() && v.pending.front()->numBytes() <= v.credits){
      SnapprPacket* pkt = v.pending.front();
      port_queue_.push(pkt);
      v.credits -= pkt->numBytes();
      v.pending.pop();
    }
  }

  SnapprPacket* pop(uint64_t cycle) override {
    SnapprPacket* pkt = port_queue_.front();
    port_debug("FIFO %p VL %d popping packet", this, pkt->virtualLane());
    port_queue_.pop();
    return pkt;
  }

  bool empty() const override {
    return port_queue_.empty();
  }

 private:
  std::vector<VirtualLane> vls_;
  std::queue<SnapprPacket*> port_queue_;
};

struct ScatterPortArbitrator : public SnapprPortArbitrator
{
  struct VirtualLane {
    uint32_t credits;
    std::queue<SnapprPacket*> pending;
  };

 public:
  void insert(uint64_t cycle, SnapprPacket *pkt) override {
    VirtualLane& vl = vls_[pkt->virtualLane()];
    if (vl.credits >= pkt->numBytes()){
      port_queue_.emplace(pkt);
      vl.credits -= pkt->numBytes();
    } else {
      vl.pending.push(pkt);
    }
  }

  SnapprPacket* pop(uint64_t cycle) override {
    SnapprPacket* pkt = port_queue_.top();
    port_queue_.pop();
    return pkt;
  }

  void addCredits(int vl, uint32_t credits) override {
    VirtualLane& v = vls_[vl];
    v.credits += credits;
    while (!v.pending.empty() && v.pending.front()->numBytes() <= v.credits){
      SnapprPacket* pkt = v.pending.front();
      v.pending.pop();
      port_queue_.emplace(pkt);
      v.credits -= pkt->numBytes();
    }
  }

  bool empty() const override {
    return port_queue_.empty();
  }

 private:
  struct priority_is_lower {
    bool operator()(const SnapprPacket* l,
                    const SnapprPacket* r) const {
      //prioritize packets with lower offsets
      return l->offset() > r->offset();
    }
  };

  std::vector<VirtualLane> vls_;

  std::priority_queue<SnapprPacket*, std::vector<SnapprPacket*>,
      priority_is_lower> port_queue_;
};

struct WRR_PortArbitrator : public SnapprPortArbitrator
{
 public:
  void insert(uint64_t cycle, SnapprPacket *pkt) override {
    int vl = pkt->virtualLane();
    VirtualLane& v = vls_[vl];
    if (v.pending.empty()){ //always enough credits when empty - better be
      uint64_t deadline = cycle + pkt->numBytes() * v.max_byte_delay;
      port_queue_.emplace(deadline, vl);
    }
    vls_[vl].pending.push(pkt);
  }

  SnapprPacket* pop(uint64_t cycle) override {
#if SSTMAC_SANITY_CHECK
    if (port_queue_.empty()){
      spkt_abort_printf("pulling snappr packet from empty queue");
    }
#endif
    int next_vl = port_queue_.top().second;
    port_queue_.pop();

    VirtualLane& vl = vls_[next_vl];
    SnapprPacket* pkt = vl.pending.front();
    vl.pending.pop();
    vl.credits -= pkt->numBytes();

    if (!vl.pending.empty()){
      SnapprPacket* pkt = vl.pending.front();
      uint64_t deadline = cycle + pkt->numBytes() * vl.max_byte_delay;
      if (vl.credits >= pkt->numBytes()){
        port_queue_.emplace(deadline, next_vl);
      } else {
        vl.blocked_deadline = deadline;
      }
    }
    return pkt;
  }

  void addCredits(int vl, uint32_t credits) override {
    VirtualLane& v = vls_[vl];
    v.credits += credits;
    if (v.blocked_deadline){
      SnapprPacket* pkt = v.pending.front();
      if (pkt->numBytes() <= v.credits){
        port_queue_.emplace(v.blocked_deadline, vl);
        v.blocked_deadline = 0;
      }
    }
  }

  bool empty() const override {
    return port_queue_.empty();
  }


 private:
  struct VirtualLane {
    std::queue<SnapprPacket*> pending;
    uint64_t max_byte_delay;
    uint32_t credits;
    uint64_t blocked_deadline;
  };

  struct priority_is_lower {
    bool operator()(const std::pair<uint64_t,int>& l,
                    const std::pair<uint64_t,int>& r) const {
      return l.first > r.first;
    }
  };

  std::priority_queue<std::pair<uint64_t,int>,
      std::vector<std::pair<uint64_t,int>>,
      priority_is_lower> port_queue_;
  std::vector<VirtualLane> vls_;
};

}
}
