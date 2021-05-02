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

#ifndef snappr_outport_h
#define snappr_outport_h

#include <sstmac/common/stats/stat_collector.h>
#include <sstmac/common/stats/ftq_fwd.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/hardware/snappr/snappr.h>
#include <sstmac/hardware/snappr/snappr_inport.h>

namespace sstmac {
namespace hw {

struct SnapprPortArbitrator {
  SPKT_DECLARE_BASE(SnapprPortArbitrator)
  SPKT_DECLARE_CTOR(TimeDelta, SST::Params&, const std::vector<int>&)

  virtual void insert(uint64_t cycle, SnapprPacket* pkt) = 0;

  virtual void addCredits(int vl, uint32_t credits) = 0;

  virtual SnapprPacket* popDeadlockCheck(int /*vl*/){ return nullptr; }

  virtual SnapprPacket* pop(uint64_t cycle) = 0;

  virtual bool empty() const = 0;

  virtual void scale(double factor) = 0;

  virtual void setVirtualLanes(const std::vector<uint32_t>& vl_credits) = 0;

  virtual int queueLength(int vl) const = 0;

  virtual int numVirtualLanes() const = 0;

};

struct SnapprOutPort : public SubComponent {

#if SSTMAC_INTEGRATED_SST_CORE
  SST_ELI_REGISTER_SUBCOMPONENT_API(sstmac::hw::SnapprOutPort,
                                    const std::string& /*subId*/, const std::string& /*portName*/, int /*number*/,
                                    bool /*congestion*/, bool /*flow_control*/, Component* /*parent*/,
                                    const std::vector<int>& /*vls_per_qos*/)
  SST_ELI_REGISTER_SUBCOMPONENT_DERIVED(
    SnapprOutPort,
    "macro",
    "snappr_outport",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "implements a basic Snappr OutPort",
    sstmac::hw::SnapprOutPort)
#else
  SST_ELI_DECLARE_BASE(SnapprOutPort)
  SST_ELI_DECLARE_DEFAULT_INFO()
  SST_ELI_DECLARE_CTOR(uint32_t /*id*/, SST::Params& /*params*/,
                       const std::string& /*subId*/, const std::string& /*portName*/, int /*number*/,
                       bool /*congestion*/, bool /*flow_control*/, Component* /*parent*/,
                       const std::vector<int>& /*vls_per_qos*/)

  SST_ELI_REGISTER_DERIVED(
    SnapprOutPort,
    SnapprOutPort,
    "macro",
    "snappr",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "implements a basic Snappr OutPort")
#endif

  struct TailNotifier {
    virtual void notify(Timestamp,SnapprPacket*) = 0;
  };

  template <class T, class Fxn>
  struct TailNotifierDerived : public TailNotifier {
    TailNotifierDerived(T* t, Fxn f) : t_(t), f_(f) {}

    void notify(Timestamp done, SnapprPacket* pkt) override {
      (t_->*f_)(done,pkt);
    }
    T* t_;
    Fxn f_;
  };

  int dst_port;
  bool arbitration_scheduled;
  Timestamp next_free;
  Timestamp stall_start;
  Timestamp send_start;
  Timestamp last_queue_depth_collection;
  TimeDelta byte_delay;
  TimeDelta flit_overhead;
  int ftq_idle_state;
  int ftq_stalled_state;
  int ftq_active_state;
  SST::Statistics::Statistic<uint64_t>* xmit_stall;
  SST::Statistics::Statistic<uint64_t>* xmit_active;
  SST::Statistics::Statistic<uint64_t>* xmit_idle;
  SST::Statistics::Statistic<uint64_t>* bytes_sent;
  sstmac::FTQCalendar* state_ftq;
  sstmac::FTQCalendar* queue_depth_ftq;
  SnapprInPort* inports;
  EventLink::ptr link;

  std::string toString() const;

  void deadlockCheck(int vl);

  void handle(Event* ev);

  std::string portName() const {
    return portName_;
  }

  int number() const {
    return number_;
  }

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

  void handleCredit(SnapprCredit* credit);

  void scaleBuffers(double factor){
    arb_->scale(factor);
  }

  void tryToSendPacket(SnapprPacket* pkt);

  void setVirtualLanes(const std::vector<uint32_t>& credits){
    arb_->setVirtualLanes(credits);
  }

  int numVirtualLanes() const {
    return arb_->numVirtualLanes();
  }

  template <class T, class Fxn>
  void addTailNotifier(T* t, Fxn f){
    notifier_ = new TailNotifierDerived<T,Fxn>(t,f);
  }

  SnapprOutPort(uint32_t id, SST::Params& params,
                const std::string& subId, const std::string& portName, int number,
                bool congestion, bool flow_control, Component* parent,
                const std::vector<int>& vls_per_qos);

 private:
  void logQueueDepth();

  void arbitrate();

  void requestArbitration();

  void scheduleArbitration();

  SnapprPacket* popReady(){
    --total_packets_;
    return arb_->pop(parent_->now().time.ticks());
  }

  void queue(SnapprPacket* pkt){
    arb_->insert(parent_->now().time.ticks(), pkt);
    total_packets_++;
  }

  void addCredits(int vl, uint32_t credits){
    arb_->addCredits(vl, credits);
  }

  void send(SnapprPacket* pktr, Timestamp now);

  int debug_qos_;
  SnapprPortArbitrator* arb_;
  Component* parent_;
  int total_packets_;
  bool flow_control_;
  bool congestion_;
  std::string portName_;
  int number_;
  TailNotifier* notifier_;
  std::set<int> deadlocked_vls_;

};

}
}

#endif

