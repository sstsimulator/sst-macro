#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif

#include <inttypes.h>
#include <sstmac/hardware/snappr/snappr_outport.h>
#include <sstmac/common/stats/ftq.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/stats/ftq.h>
#include <sstmac/common/stats/ftq_tag.h>
#include <queue>

#define pkt_debug(...) \
  debug_printf(sprockit::dbg::snappr, "snappr port %s:%d %s", \
    portName_.c_str(), number_, sprockit::printf(__VA_ARGS__).c_str())


#define port_debug(...) \
  debug_printf(sprockit::dbg::snappr, __VA_ARGS__)

namespace sstmac {
namespace hw {

SnapprOutPort::SnapprOutPort(SST::Params& params, const std::string &arb,
                             const std::string& subId, const std::string& portName,
                             int number, TimeDelta byt_delay, bool congestion, bool flow_control,
                             Component* parent)
  : arbitration_scheduled(false), 
    byte_delay(byt_delay), 
    state_ftq(nullptr),
    queue_depth_ftq(nullptr),
    inports(nullptr),
    parent_(parent), 
    flow_control_(flow_control),
    congestion_(congestion), 
    portName_(subId), 
    number_(number),
    notifier_(nullptr)
{
  arb_ = sprockit::create<SnapprPortArbitrator>("macro", arb, byte_delay, params);
  xmit_active = parent->registerStatistic<uint64_t>(params, "xmit_active", subId);
  xmit_idle = parent->registerStatistic<uint64_t>(params, "xmit_idle", subId);
  xmit_stall = parent->registerStatistic<uint64_t>(params, "xmit_stall", subId);
  bytes_sent = parent->registerStatistic<uint64_t>(params, "bytes_sent", subId);
#if !SSTMAC_INTEGRATED_SST_CORE
  state_ftq = dynamic_cast<FTQCalendar*>(
        parent->registerMultiStatistic<int,uint64_t,uint64_t>(params, "state", subId));
  queue_depth_ftq = dynamic_cast<FTQCalendar*>(
        parent->registerMultiStatistic<int,uint64_t,uint64_t>(params, "queue_depth", subId));
#endif

  ftq_idle_state = FTQTag::allocateCategoryId("idle:" + portName);
  ftq_active_state = FTQTag::allocateCategoryId("active:" + portName);
  ftq_stalled_state = FTQTag::allocateCategoryId("stalled:" + portName);
}

void
SnapprOutPort::handle(Event *ev)
{
  handleCredit(static_cast<SnapprCredit*>(ev));
}

std::string
SnapprOutPort::toString() const
{
  return sprockit::printf("SNAPPR OutPort %d", number_);
}

void
SnapprOutPort::handleCredit(SnapprCredit* credit)
{
  addCredits(credit->virtualLane(), credit->numBytes());
  pkt_debug("crediting port=%d vl=%d with %" PRIu32" credits",
            number_, credit->virtualLane(), credit->numBytes());
  delete credit;
  if (!arbitration_scheduled && !empty()){
    requestArbitration();
  }
}

void
SnapprOutPort::send(SnapprPacket* pkt, Timestamp now)
{
#if SSTMAC_SANITY_CHECK
  if (next_free > now){
    spkt_abort_printf("Internal error: port %s:%d sending packet at time %llu < %llu",
                      portName_.c_str(), number_, now.time.ticks(), next_free.time.ticks());
  }
#endif

  if (!stall_start.empty()){
    TimeDelta stall_time = now - stall_start;
    xmit_stall->addData(stall_time.ticks());
#if !SSTMAC_INTEGRATED_SST_CORE
    if (state_ftq){
      state_ftq->addData(ftq_stalled_state, stall_start.time.ticks(), stall_time.ticks());
    }
#endif
    if (stall_start > next_free){
      //we also have idle time
      TimeDelta idle_time = stall_start -next_free;
      xmit_idle->addData(idle_time.ticks());
#if !SSTMAC_INTEGRATED_SST_CORE
      if (state_ftq){
        state_ftq->addData(ftq_idle_state, next_free.time.ticks(), idle_time.ticks());
      }
#endif
    }
    stall_start = Timestamp();
  } else if (now > next_free){
    TimeDelta idle_time = now - next_free;
    xmit_idle->addData(idle_time.ticks());
#if !SSTMAC_INTEGRATED_SST_CORE
    if (state_ftq){
      state_ftq->addData(ftq_idle_state, next_free.time.ticks(), idle_time.ticks());
    }
#endif
  }

  TimeDelta time_to_send = pkt->numBytes() * byte_delay;
  bytes_sent->addData(pkt->numBytes());
  xmit_active->addData(time_to_send.ticks());
#if !SSTMAC_INTEGRATED_SST_CORE
  if (state_ftq){
    state_ftq->addData(ftq_active_state, now.time.ticks(), time_to_send.ticks());
  }
#endif
  next_free = now + time_to_send;
  pkt->setTimeToSend(time_to_send);
  pkt->accumulateCongestionDelay(now);
#if SSTMAC_SANITY_CHECK
  if (!link){
    spkt_abort_printf("trying send on null link going to %d: %s",
                      pkt->toaddr(), pkt->toString().c_str());
  }
#endif
  link->send(pkt);

  if (flow_control_ && inports){
    auto& inport = inports[pkt->inport()];
    auto* credit = new SnapprCredit(pkt->byteLength(), pkt->inputVirtualLane(), inport.src_outport);
    pkt_debug("sending credit to port=%d on vl=%d at t=%8.4e: %s",
              inport.src_outport, pkt->inputVirtualLane(), next_free.sec(), pkt->toString().c_str());
    inport.link->send(time_to_send, credit);
  } else {
    //immediately add the credits back - we don't worry about credits here
    addCredits(pkt->virtualLane(), pkt->byteLength());
  }

  if (notifier_ && pkt->isTail()){
    notifier_->notify(next_free, pkt);
  }

  pkt_debug("packet leaving port=%d vl=%d at t=%8.4e: %s",
            number_, pkt->virtualLane(), next_free.sec(), pkt->toString().c_str());
  if (ready()){
    scheduleArbitration();
  }
}

void
SnapprOutPort::scheduleArbitration()
{
#if SSTMAC_SANITY_CHECK
  if (arbitration_scheduled){
    spkt_abort_printf("arbitration already scheduled on port %s:%d", portName_.c_str(), number_);
  }
  if (queueLength() == 0){
    spkt_abort_printf("scheduling arbitration on port with nothing queued");
  }
#endif
  pkt_debug("scheduling arbitrate from port %d at t=%8.4e with %d queued",
            number_, next_free.sec(), queueLength());
  //schedule this port to pull another packet
  auto* ev = newCallback(this, &SnapprOutPort::arbitrate);
  parent_->sendExecutionEvent(next_free, ev);
  arbitration_scheduled = true;
}

void
SnapprOutPort::requestArbitration()
{
#if SSTMAC_SANITY_CHECK
  if (empty()){
    spkt_abort_printf("SnapprSwitch::arbitrate: incorrectly requesting arbitrate from empty port %s:%d",
                      portName_.c_str(), number_);
  }
  if (arbitration_scheduled){
    spkt_abort_printf("SnapprSwitch::arbitrate: incorrectly requesting arbitrate from port %s:%d with arbitrate scheduled already",
                      portName_.c_str(), number_);
  }
#endif
  Timestamp now_ = parent_->now();
  if (next_free > now_){
    scheduleArbitration();
  } else {
    arbitrate();
  }
}

void
SnapprOutPort::arbitrate()
{
#if SSTMAC_SANITY_CHECK
  if (empty()){
    spkt_abort_printf("SnapprOutPort::arbitrate: incorrectly arbitrate from empty port %d",
                      number_);
  }
  if (next_free > parent_->now()){
    spkt_abort_printf("SnapprOutPort::arbitrate: arbitrating before port %d is free to send",
                      number_);
  }
#endif

  arbitration_scheduled = false;
  if (ready()){
    logQueueDepth();
    pkt_debug("arbitrating packet from port %d with %d queued",
              number_, queueLength());
    SnapprPacket* pkt = popReady();
    send(pkt, parent_->now());
  } else {
    if (stall_start.empty()){
      stall_start = parent_->now();
    }
    pkt_debug("insufficient credits to send on port %d with %d queued",
              number_, queueLength());
  }
}

void
SnapprOutPort::logQueueDepth()
{
#if !SSTMAC_INTEGRATED_SST_CORE
  if (queue_depth_ftq){
    Timestamp now = parent_->now();
    TimeDelta dt = now - last_queue_depth_collection;
    queue_depth_ftq->addData(queueLength(), last_queue_depth_collection.time.ticks(), dt.ticks());
    last_queue_depth_collection = now;
  }
#endif
}

void
SnapprOutPort::tryToSendPacket(SnapprPacket* pkt)
{
  pkt_debug("trying to send payload %s on inport %d:%d going to port %d:%d:%d",
            pkt->toString().c_str(), pkt->inport(), pkt->inputVirtualLane(),
            pkt->nextPort(), pkt->virtualLane(), pkt->deadlockVC());

  Timestamp now = parent_->now();
  pkt->setArrival(now);
  if (!congestion_){
    TimeDelta time_to_send = pkt->numBytes() * byte_delay;
    pkt->setTimeToSend(time_to_send);
    link->send(pkt);
  } else {
    logQueueDepth();
    queue(pkt);
    pkt_debug("incoming packet on port=%d vl=%d -> queue=%d",
              number_, pkt->virtualLane(), queueLength());
    if (!arbitration_scheduled){
      requestArbitration();
    }
  }
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

  FifoPortArbitrator(TimeDelta  /*link_byte_delay*/, SST::Params&  /*params*/){}

  void insert(uint64_t  /*cycle*/, SnapprPacket *pkt) override {
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

  void scale(double factor) override {
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
      port_debug("FIFO %p VL %d now has enough credits to send packet %s",
                 this, pkt->virtualLane(), pkt->toString().c_str());
      port_queue_.push(pkt);
      v.credits -= pkt->numBytes();
      v.pending.pop();
    }
  }

  SnapprPacket* pop(uint64_t  /*cycle*/) override {
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
  ScatterPortArbitrator(SST::Params&  /*params*/){
    //nothing to do
  }

  void insert(uint64_t  /*cycle*/, SnapprPacket *pkt) override {
    VirtualLane& vl = vls_[pkt->virtualLane()];
    if (vl.credits >= pkt->numBytes()){
      port_queue_.emplace(pkt);
      vl.credits -= pkt->numBytes();
    } else {
      vl.pending.push(pkt);
    }
  }

  SnapprPacket* pop(uint64_t  /*cycle*/) override {
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
  SPKT_REGISTER_DERIVED(
    SnapprPortArbitrator,
    WRR_PortArbitrator,
    "macro",
    "wrr",
    "implements a WRR strategy for queuing packets with bandwidth sharing weights")

  WRR_PortArbitrator(TimeDelta link_byte_delay, SST::Params& params){
    std::vector<double> weights;
    params.find_array("vl_weights", weights);
    // Set W = BW weight for VL
    // Set D = max delay until send starts
    // Set T = time to send
    // T / (T + D) = W
    // T = W*T + W*D
    // (1-W)*T = W*D
    // D = (1-W) * T / W
    vls_.resize(weights.size());
    for (int vl=0; vl < weights.size(); ++vl){
      VirtualLane& v = vls_[vl];
      double weight = weights[vl];
      double weight_factor = (1 - weight) / weight;
      v.max_byte_delay = weight_factor * link_byte_delay.ticks();
      v.blocked_deadline = 0;
    }
  }

  void setVirtualLanes(int num_vl, uint32_t total_credits) override {
    if (num_vl != int(vls_.size())){
      spkt_abort_printf("WRR arbitrator got %d VLs, but weight vector of size %d",
                        num_vl, int(vls_.size()));
    }
    uint32_t credits_per = total_credits / num_vl;
    for (int vl=0; vl < num_vl; ++vl){
      vls_[vl].credits = credits_per;
    }
  }

  int queueLength(int vl) const override {
    return vls_[vl].pending.size();
  }

  void scale(double factor) override {
    for (VirtualLane& vl : vls_){
      vl.credits *= factor;
      //if bandwidth is scaled up, shrink my max delay
      vl.max_byte_delay /= factor;
    }
  }

  void insert(uint64_t cycle, SnapprPacket *pkt) override {
    int vl = pkt->virtualLane();
    VirtualLane& v = vls_[vl];
    if (v.pending.empty()){ //always enough credits when empty - better be
      uint64_t deadline = cycle + pkt->numBytes() * v.max_byte_delay;
      port_debug("WRR %p VL %d is empty and emplacing at deadline=%" PRIu64 " on cycle=%" PRIu64 ": %s",
                 this, vl, deadline, cycle, pkt->toString().c_str());
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

    port_debug("WRR %p VL %d sending on cycle=%" PRIu64 ": %s",
               this, next_vl, cycle, pkt->toString().c_str());

    if (!vl.pending.empty()){
      SnapprPacket* pkt = vl.pending.front();
      uint64_t deadline = cycle + pkt->numBytes() * vl.max_byte_delay;
      if (vl.credits >= pkt->numBytes()){
        port_debug("WRR %p VL %d has enough credits=%u to emplace at deadline=%" PRIu64
                   " on cycle=%" PRIu64 ": %s",
                   this, next_vl, deadline, cycle, vl.credits, pkt->toString().c_str());
        port_queue_.emplace(deadline, next_vl);
      } else {
        port_debug("WRR %p VL %d has insufficient credits=%u for deadline=%" PRIu64
                   " on cycle=%" PRIu64 ": %s",
                   this, next_vl, vl.credits, deadline, cycle, pkt->toString().c_str(), deadline);
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
        port_debug("WRR %p VL %d now has enough credits to emplace %s for deadline %" PRIu64,
                   this, vl, pkt->toString().c_str(), v.blocked_deadline);
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

