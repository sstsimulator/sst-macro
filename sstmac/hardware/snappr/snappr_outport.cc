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

#include <unusedvariablemacro.h>

#define pkt_debug(...) \
  debug_printf(sprockit::dbg::snappr, "snappr port %s:%d %s", \
    portName_.c_str(), number_, sprockit::sprintf(__VA_ARGS__).c_str())


#define port_debug(...) \
  debug_printf(sprockit::dbg::snappr, __VA_ARGS__)

namespace sstmac {
namespace hw {

SnapprOutPort::SnapprOutPort(uint32_t id, SST::Params& params, const std::string &arb,
                             const std::string& subId, const std::string& portName,
                             int number, TimeDelta byt_delay, bool congestion, bool flow_control,
                             Component* parent, const std::vector<int>& vls_per_qos)
  : arbitration_scheduled(false), 
    byte_delay(byt_delay), 
    state_ftq(nullptr),
    queue_depth_ftq(nullptr),
    inports(nullptr),
    parent_(parent), 
    total_packets_(0),
    flow_control_(flow_control),
    congestion_(congestion), 
    portName_(subId), 
    number_(number),
    notifier_(nullptr),
    SubComponent(id, portName, parent)
{
  arb_ = sprockit::create<SnapprPortArbitrator>("macro", arb, byte_delay, params, vls_per_qos);
  xmit_active = registerStatistic<uint64_t>(params, "xmit_active", subId);
  xmit_idle = registerStatistic<uint64_t>(params, "xmit_idle", subId);
  xmit_stall = registerStatistic<uint64_t>(params, "xmit_stall", subId);
  bytes_sent = registerStatistic<uint64_t>(params, "bytes_sent", subId);
#if !SSTMAC_INTEGRATED_SST_CORE
  state_ftq = dynamic_cast<FTQCalendar*>(
        parent->registerMultiStatistic<int,uint64_t,uint64_t>(params, "state", subId));
  queue_depth_ftq = dynamic_cast<FTQCalendar*>(
        parent->registerMultiStatistic<int,uint64_t,uint64_t>(params, "queue_depth", subId));
#endif
  ftq_idle_state = FTQTag::allocateCategoryId("idle:" + portName);
  ftq_active_state = FTQTag::allocateCategoryId("active:" + portName);
  ftq_stalled_state = FTQTag::allocateCategoryId("stalled:" + portName);

  //default is to assume no flit overhead
  uint32_t flit_size = params.find<SST::UnitAlgebra>("flit_size", "0").getRoundedValue();
  flit_overhead = flit_size * byte_delay;
}

void
SnapprOutPort::handle(Event *ev)
{
  handleCredit(static_cast<SnapprCredit*>(ev));
}

std::string
SnapprOutPort::toString() const
{
  return sprockit::sprintf("SNAPPR OutPort %d", number_);
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
  next_free = now + time_to_send + flit_overhead;
  pkt->setTimeToSend(time_to_send);
  pkt->accumulateCongestionDelay(now);
#if SSTMAC_SANITY_CHECK
  if (!link){
    spkt_abort_printf("trying send on null link going to %d: %s",
                      pkt->toaddr(), pkt->toString().c_str());
  }
#endif
  link->send(flit_overhead, pkt);

  if (flow_control_){
    if (inports){
      auto& inport = inports[pkt->inport()];
      auto* credit = new SnapprCredit(pkt->byteLength(), pkt->inputVirtualLane(), inport.src_outport);
      pkt_debug("sending credit to port=%d on vl=%d at t=%8.4e: %s",
                inport.src_outport, pkt->inputVirtualLane(), next_free.sec(), pkt->toString().c_str());
      inport.link->send(time_to_send + flit_overhead, credit);
    }
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
SnapprOutPort::deadlockCheck(SSTMAC_MAYBE_UNUSED int vl)
{
#if !SSTMAC_INTEGRATED_SST_CORE
  auto iter = deadlocked_vls_.find(vl);
  if (iter != deadlocked_vls_.end()){
    spkt_abort_printf("Found deadlocked VL %d", vl);
  }
  deadlocked_vls_.insert(vl);

  auto* pkt = arb_->popDeadlockCheck(vl);
  if (pkt){
    std::cerr << "Deadlocked on PORT=" << number_ <<   " VL=" << vl << ": " << pkt->toString() << std::endl;
    pkt->setDeadlocked();
    link->deliver(pkt);
  }
#endif
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

  FifoPortArbitrator(TimeDelta  /*link_byte_delay*/, SST::Params&  /*params*/,
                     const std::vector<int>& /*vls_per_qos*/){}

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

  SnapprPacket* popDeadlockCheck(int vl) override {
    VirtualLane& v = vls_[vl];
    if (!v.pending.empty()){
      SnapprPacket* pkt = v.pending.front();
      return pkt;
    } else {
      return nullptr;
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

  void setVirtualLanes(const std::vector<uint32_t>& credits) override {
    vls_.resize(credits.size());
    for (int i=0; i < vls_.size(); ++i){
      vls_[i].credits = credits[i];
    }
  }

  int numVirtualLanes() const override {
    return vls_.size();
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

struct WRR_PortArbitrator : public SnapprPortArbitrator
{
 private:
  struct VirtualLane {

   typedef enum {
     QosBandwidthMin,
     QosBandwidthMax,
     QosNone
   } SelectionType;

   VirtualLane(int num, SelectionType s, int prior, uint64_t delay) :
     sel_type(s),
     byte_delay(delay),
     stalled(false),
     next_free(0),
     credits(0),
     priority(prior),
     number(num)
   {
   }

   VirtualLane(int num, SelectionType s, int prior) :
     VirtualLane(num, s, prior, std::numeric_limits<uint64_t>::max())
   {
   }

   SelectionType sel_type;

   std::queue<SnapprPacket*> pending;

   /** For bandwidth minimum, this is the max amount a byte can be delayed
    *  on a switch and still preserve the minimum.
    *  For bandwidth maximum, this is the min amount a byte must be delayed
    *  after the previous byte and still stay under the cap.
    *  For no QoS, this has no meaning.
    */
   uint64_t byte_delay;


   bool stalled;

   /** For bandwidth minimum, this is
    *  the cycle after which a VL will go below its bandwidth minimum.
    *  For bandwidth maximum, this is
    *  the cycle a VL has to wait until in order to stay below its bandwidth cap
    *  For no Qos, this has no meaning.
    *
    *  All strategies use this as a flag to indicate a stalled VL.
    *  Zero indicates no stalled packets. Non-zero indicates stalled packets.
    */

   uint64_t next_free;

   uint32_t credits;

   int priority;

   int number;
 };

 struct blockedUntilCompare {
   bool operator()(const VirtualLane* l,
                   const VirtualLane* r) const {
     return l->next_free > r->next_free;
   }
 };

 std::priority_queue<VirtualLane*, std::vector<VirtualLane*>,
                     blockedUntilCompare> bw_cap_queue_;

 struct priorityDeadlineCompare {
   bool operator()(const VirtualLane* l,
                   const VirtualLane* r) const {
     if (l->priority == r->priority){
       //those with an earlier next free come later (higher priority)
       return l->next_free > r->next_free;
     } else {
       return l->priority < r->priority;
     }
   }
 };

 std::priority_queue<VirtualLane*, std::vector<VirtualLane*>,
     priorityDeadlineCompare> port_queue_;
 std::vector<VirtualLane> vls_;

 uint64_t link_byte_delay_;

 public:
  SPKT_REGISTER_DERIVED(
    SnapprPortArbitrator,
    WRR_PortArbitrator,
    "macro",
    "wrr",
    "implements a WRR strategy for queuing packets with bandwidth sharing weights")

  WRR_PortArbitrator(TimeDelta link_byte_delay, SST::Params& params,
                     const std::vector<int>& vls_per_qos) :
    link_byte_delay_(link_byte_delay.ticks())
  {
    std::vector<double> weights;
    if (params.contains("vl_weights")){
      params.find_array("vl_weights", weights);
    } else if (params.contains("qos_weights")) {
      std::vector<double> qos_weights;
      params.find_array("qos_weights", qos_weights);
      for (int q=0; q < qos_weights.size(); ++q){
        for (int vl=0; vl < vls_per_qos[q]; ++vl){
          weights.push_back(qos_weights[q]);
        }
      }
    } else {
      spkt_abort_printf("No VL or QOS weights given to port arbitrator");
    }

    std::vector<std::string> types;
    const std::map<std::string, VirtualLane::SelectionType> sel_type_map;
    if (params.contains("vl_types")){
      params.find_array("vl_types", types);
    } else if (params.contains("qos_types")){
      std::vector<std::string> qos_types;
      params.find_array("qos_types", qos_types);
      for (int q=0; q < qos_types.size(); ++q){
        for (int vl=0; vl < vls_per_qos[q]; ++vl){
          types.push_back(qos_types[q]);
        }
      }
    } else {
      types.resize(weights.size(), std::string("min"));
    }
    if (weights.size() != types.size()){
      spkt_abort_printf("Given %d VL weights, but %d VL types",
                        int(weights.size()), int(types.size()));
    }


    std::vector<int> priorities;
    if (params.contains("vl_priorities")){
      params.find_array("vl_priorities", priorities);
    } else if (params.contains("qos_priorities")) {
      std::vector<int> qos_priors;
      params.find_array("qos_priorities", qos_priors);
      for (int q=0; q < qos_priors.size(); ++q){
        for (int vl=0; vl < vls_per_qos[q]; ++vl){
          priorities.push_back(qos_priors[q]);
        }
      }
    } else {
      //just give everything priority zero
      priorities.resize(weights.size(), 0);
    }
    if (weights.size() != priorities.size()){
      spkt_abort_printf("Given %d VL weights, but %d VL priorities",
                        int(weights.size()), int(priorities.size()));
    }

    const std::map<std::string, VirtualLane::SelectionType> type_map {
      {"min", VirtualLane::QosBandwidthMin},
      {"max", VirtualLane::QosBandwidthMax},
      {"none", VirtualLane::QosNone},
    };

    vls_.reserve(weights.size());
    for (int vl=0; vl < types.size(); ++vl){
      if (types[vl] == "min"){
        // Set W = BW weight for VL
        // Set D = max delay until send starts
        // Set T = time to send
        // T / (T + D) = W
        // T = W*T + W*D
        // (1-W)*T = W*D
        // D = (1-W) * T / W
        double weight = weights[vl];
        double weight_factor = (1 - weight) / weight;
        vls_.emplace_back(vl, VirtualLane::QosBandwidthMin,
          priorities[vl],
          weight_factor * link_byte_delay.ticks());
      } else if (types[vl] == "max"){
        double bandwidth_cap = weights[vl];
        double gap_needed = 1.0 / bandwidth_cap - 1.0;
        uint64_t extra_byte_delay = link_byte_delay.ticks() * gap_needed;
        vls_.emplace_back(vl, VirtualLane::QosBandwidthMax,
                          priorities[vl], extra_byte_delay);
      } else if (types[vl] == "none"){
        vls_.emplace_back(vl, VirtualLane::QosNone,
                          priorities[vl]);
      } else {
        spkt_abort_printf("Bad virtual lane type %s given", types[vl].c_str());
      }
    }
  }

  SnapprPacket* popDeadlockCheck(int vl) override {
    VirtualLane& v = vls_[vl];
    if (!v.pending.empty()){
      SnapprPacket* pkt = v.pending.front();
      return pkt;
    } else {
      return nullptr;
    }
  }

  void setVirtualLanes(const std::vector<uint32_t>& credits) override {
    if (credits.size() != vls_.size()){
      spkt_abort_printf("WRR arbitrator got %d VLs, but weight vector of size %d",
                        int(credits.size()), int(vls_.size()));
    }
    for (int i=0; i < vls_.size(); ++i){
      vls_[i].credits = credits[i];
    }
  }

  int numVirtualLanes() const override {
    return vls_.size();
  }

  int queueLength(int vl) const override {
    return vls_[vl].pending.size();
  }

  void scale(double factor) override {
    for (VirtualLane& vl : vls_){
      vl.credits *= factor;
      //if bandwidth is scaled up, shrink my max delay
      vl.byte_delay /= factor;
    }
  }

  void insertNoQoS(uint64_t /*cycle*/, SnapprPacket *pkt) {
    int vl= pkt->virtualLane();
    VirtualLane& v = vls_[vl];
    v.next_free = std::numeric_limits<uint64_t>::max();
    //there is no special packet deadline here
    port_queue_.emplace(&v);
    if (v.credits < pkt->numBytes()){
      spkt_abort_printf("WRR %p VL %d no QOS - credits are insufficient",
                        this, pkt->virtualLane());
    }
    v.credits -= pkt->numBytes();
  }

  void insertBandwidthMin(uint64_t cycle, SnapprPacket *pkt) {
    VirtualLane& v = vls_[pkt->virtualLane()];
    uint64_t deadline = cycle + pkt->numBytes() * v.byte_delay;
    v.next_free = deadline;
    port_debug("WRR %p VL %d is empty and emplacing at deadline=%" PRIu64 " on cycle=%" PRIu64 ": %s",
               this, pkt->virtualLane(), deadline, cycle, pkt->toString().c_str());
    if (v.credits < pkt->numBytes()){
      spkt_abort_printf("WRR %p VL %d bandwidth min credits are insufficient",
                        this, pkt->virtualLane());
    }
    port_queue_.emplace(&v);
    v.credits -= pkt->numBytes();
  }

  void insertBandwidthMax(uint64_t cycle, SnapprPacket* pkt) {
    //there might be a bandwidth cap here to negotiate
    VirtualLane& v = vls_[pkt->virtualLane()];
    if (v.credits < pkt->numBytes()){
      spkt_abort_printf("WRR %p VL %d bandwidth max credits are insufficient",
                        this, pkt->virtualLane());
    }
    v.credits -= pkt->numBytes();
    if (v.next_free > cycle){
      port_debug("WRR %p VL %d is empty and emplacing in cap queue at next_free=%" PRIu64 " on cycle=%" PRIu64 ": %s",
                 this, pkt->virtualLane(), v.next_free, cycle, pkt->toString().c_str());
      bw_cap_queue_.emplace(&v);
    } else {
      port_debug("WRR %p VL %d is empty and emplacing in port queue at next_free=%" PRIu64 " on cycle=%" PRIu64 ": %s",
                 this, pkt->virtualLane(), v.next_free, cycle, pkt->toString().c_str());
      v.next_free = cycle;
      port_queue_.emplace(&v);
    }
  }

  void insert(uint64_t cycle, SnapprPacket *pkt) override {
    int vl = pkt->virtualLane();
    VirtualLane& v = vls_[vl];
    if (v.pending.empty()){
      if (v.credits >= pkt->numBytes()){
        port_debug("WRR %p VL %d inserting on empty, now has %d credits", 
                   this, v.number, v.credits);
        switch(v.sel_type){
          case VirtualLane::QosBandwidthMax:
            insertBandwidthMax(cycle, pkt);
            break;
          case VirtualLane::QosBandwidthMin:
            insertBandwidthMin(cycle, pkt);
            break;
          case VirtualLane::QosNone:
            insertNoQoS(cycle, pkt);
            break;
        }
      } else {
        v.stalled = true;
        port_debug("WRR %p VL %d is waiting on more than %d credits - add to pending", this, v.number, v.credits);
      }
    }
    v.pending.push(pkt);
  }

  void popNextNoQos(uint64_t cycle, VirtualLane* vl){
    SnapprPacket* pkt = vl->pending.front();
    if (vl->credits >= pkt->numBytes()){
      //the deadline can be whatever we set
      vl->next_free = cycle;
      vl->credits -= pkt->numBytes();
      port_queue_.emplace(vl);
    } else {
      vl->stalled = true;
    }
  }

  void advanceBandwidthMax(uint64_t cycle, SnapprPacket* prev, VirtualLane* vl){
    //if we waited longer than we minimimally needed to on the last packet
    //cycle is guaranteed to be past last deadline
    //suppose our bandwidth cap is 0.8 and a packet takes 800 cycles
    //we would usually need to wait an extra 200 cycles after each packet to not violate
    //our bandwidth cap. If we waited 300 cycles, we accrued a 100 cycle deficit
    //of waiting longer than we needed to
    uint64_t deficit = cycle > vl->next_free ? cycle - vl->next_free : 0;
    //normally we would have to wait this long after each packet to stay within our cap
    uint64_t normal_waiting_period = prev->numBytes() * vl->byte_delay;
    //if we have a bandwidth deficit, our actual waiting period is shorter
    //continuing the example above, if our normal period is 200 cycles
    //but we have a 100 cycle deficit, then we actually only need to wait 100 cycles
    uint64_t prev_pkt_delay = prev->numBytes() * link_byte_delay_;
    vl->next_free = deficit >= normal_waiting_period
        ? cycle + prev_pkt_delay
        : cycle + prev_pkt_delay + normal_waiting_period - deficit;
    port_debug("WRR %p VL %d advance to next_free=%" PRIu64 " on cycle=%" PRIu64 ,
               this, vl->number, vl->next_free, cycle, vl->credits);
  }

  void popNextBandwidthMax(uint64_t cycle, VirtualLane* vl){
    SnapprPacket* next = vl->pending.front();
    if (vl->credits >= next->numBytes()){
      //and if I have enough credits, put me in for arbitration
      bw_cap_queue_.emplace(vl);
      port_debug("WRR %p VL %d has enough credits=%u to emplace in cap queue at next_free=%" PRIu64
                 " on cycle=%" PRIu64 ": %s",
                 this, vl->number, vl->credits, vl->next_free, cycle, next->toString().c_str());
      vl->credits -= next->numBytes();
    } else {
      //otherwise
      vl->stalled = true;
      port_debug("WRR %p VL %d has insufficient credits=%u for next_free=%" PRIu64
                 " on cycle=%" PRIu64 ": %s",
                 this, vl->number, vl->credits, vl->next_free, cycle, next->toString().c_str());
    }
  }

  void popNextBandwidthMin(uint64_t cycle, VirtualLane* vl){
    SnapprPacket* pkt = vl->pending.front();
    uint64_t deadline = cycle + pkt->numBytes() * vl->byte_delay;
    if (vl->credits >= pkt->numBytes()){
      port_debug("WRR %p VL %d has enough credits=%u to emplace at deadline=%" PRIu64
                 " on cycle=%" PRIu64 ": %s",
                 this, vl->number, vl->credits, deadline, cycle, pkt->toString().c_str());
      vl->next_free = deadline;
      vl->credits -= pkt->numBytes();
      port_queue_.emplace(vl);
    } else {
      port_debug("WRR %p VL %d has insufficient credits=%u for deadline=%" PRIu64
                 " on cycle=%" PRIu64 ": %s",
                 this, vl->number, vl->credits, deadline, cycle, pkt->toString().c_str());
      vl->stalled = true;
      vl->next_free = deadline;
    }
  }

  SnapprPacket* pop(uint64_t cycle) override {
#if SSTMAC_SANITY_CHECK
    if (bw_cap_queue_.empty() && port_queue_.empty()){
      spkt_abort_printf("WRR %p pulling snappr packet from empty queue", this);
    }
#endif
    while (!bw_cap_queue_.empty() && bw_cap_queue_.top()->next_free <= cycle){
      VirtualLane* vl = bw_cap_queue_.top();
      port_queue_.emplace(vl);
      bw_cap_queue_.pop();
    }

    VirtualLane* vl = nullptr;
    if (port_queue_.empty()){
      //just take the top packet from the bw_cap_queue
      vl = bw_cap_queue_.top();
      //port_queue_.emplace(vl);
      bw_cap_queue_.pop();
    } else {
      vl = port_queue_.top();
      port_queue_.pop();
    }

#if SSTMAC_SANITY_CHECK
    if (vl->pending.empty()){
      spkt_abort_printf("WRR %p VL %d is popping next, but pending queue is empty",
                       this, vl->number);
    }
#endif

    SnapprPacket* pkt = vl->pending.front();
    vl->pending.pop();

    port_debug("WRR %p VL %d sending on cycle=%" PRIu64 ": %s",
               this, vl->number, cycle, pkt->toString().c_str());

    switch(vl->sel_type){
    case VirtualLane::QosBandwidthMax:
      advanceBandwidthMax(cycle, pkt, vl);
      break;
    default:
      break;
    }

    if (!vl->pending.empty()){
      switch (vl->sel_type){
      case VirtualLane::QosNone:
        popNextNoQos(cycle, vl);
        break;
      case VirtualLane::QosBandwidthMin:
        popNextBandwidthMin(cycle, vl);
        break;
      case VirtualLane::QosBandwidthMax:
        popNextBandwidthMax(cycle, vl);
        break;
      }
    }
    return pkt;
  }

  /**
   * @brief unstallVLNone Do the actions need to free a virtual channel to send again.
   *                      after a credit stall for a virtual lane that is not configured
   *                      with a bandwidth cap or a bandwidth minimum
   * @param vl
   */
  void unstallNoQoS(VirtualLane* vl){
    port_queue_.emplace(vl);
  }

  void unstallBandwidthMax(VirtualLane* vl){
    port_debug("WRR %p VL %d now has enough credits for next_free %" PRIu64,
               this, vl->number, vl->next_free);
    bw_cap_queue_.emplace(vl);
  }

  void unstallBandwidthMin(VirtualLane* vl){
    port_debug("WRR %p VL %d now has enough credits for deadline %" PRIu64,
               this, vl->number, vl->next_free);
    port_queue_.emplace(vl);
  }

  void addCredits(int vl, uint32_t credits) override {
    VirtualLane& v = vls_[vl];
    port_debug("WRR %p VL %d adding %" PRIu32 " credits to existing=%" PRIu32,
               this, vl, credits, v.credits)
    v.credits += credits;
    if (v.stalled){
#if SSTMAC_SANITY_CHECK
      if (v.pending.empty()){
        spkt_abort_printf("WRR %p VL %d is stalled, but pending queue is empty",
                          this, vl);
      }
#endif
      SnapprPacket* pkt = v.pending.front();
      if (pkt->numBytes() <= v.credits){
       switch(v.sel_type){
        case VirtualLane::QosNone:
          unstallNoQoS(&v);
          break;
        case VirtualLane::QosBandwidthMin:
          unstallBandwidthMin(&v);
          break;
        case VirtualLane::QosBandwidthMax:
          unstallBandwidthMax(&v);
          break;
       }
       v.credits -= pkt->numBytes();
       v.stalled = false;
      }
    }
  }

  bool empty() const override {
    return port_queue_.empty() && bw_cap_queue_.empty();
  }
};

}
}

