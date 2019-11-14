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

#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/sculpin/sculpin_switch.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/common/event_callback.h>
#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/factory.h>
#endif
RegisterNamespaces("switch", "router", "xbar", "link");

RegisterKeywords(
{ "latency", "latency to traverse a portion of the switch - sets both credit/send" },
{ "bandwidth", "the bandwidth of a given link sending" },
{ "congestion", "whether to include congestion modeling on switches" },
{ "filter_stat_source", "list of specific source nodes to collect statistics for" },
{ "filter_stat_destination", "list of specific destination nodes to collect statistis for" },
{ "highlight_stat_source", "list of specific source nodes to highlight traffic from" },
{ "highlight_scale", "the scale factor to use for highlighting" },
{ "vtk_flicker", "whether to 'flicker' when packets leave/arrive" },
);


#define pkt_debug(...) \
  debug_printf(sprockit::dbg::sculpin, "sculpin switch %d: %s", \
    int(addr()), sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

SculpinSwitch::SculpinSwitch(uint32_t id, SST::Params& params) :
  NetworkSwitch(id, params),
  router_(nullptr),
  congestion_(true)
{
  SST::Params rtr_params = params.find_scoped_params("router");
  rtr_params.insert("id", std::to_string(my_addr_));
  router_ = sprockit::create<Router>(
   "macro", rtr_params.find<std::string>("name"), rtr_params, top_, this);

  congestion_ = params.find<bool>("congestion", true);
  vtk_flicker_ = params.find<bool>("vtk_flicker", true);

  SST::Params link_params = params.find_scoped_params("link");
  link_bw_ = link_params.find<SST::UnitAlgebra>("bandwidth").getValue().toDouble();

  // Ensure topology is set
  Topology::staticTopology(params);

  //vtk_ = registerStatistic<uint64_t,int,double,int>("traffic_intensity", getName());
  //if (vtk_) vtk_->configure(my_addr_, top_);

  ports_.resize(top_->maxNumPorts());
  for (int i=0; i < ports_.size(); ++i){
    ports_[i].id = i;
    ports_[i].seqnum = 0;
  }
  initLinks(params);

  if (params.contains("filter_stat_source")){
    std::vector<int> filter;
    params.find_array("filter_stat_source", filter);
    for (int src : filter){
      src_stat_filter_.insert(src);
    }
  }

  if (params.contains("filter_stat_destination")){
    std::vector<int> filter;
    params.find_array("filter_stat_destination", filter);
    for (int dst : filter) dst_stat_filter_.insert(dst);
  }

  if (params.contains("highlight_stat_source")){
    std::vector<int> filter;
    params.find_array("highlight_stat_source", filter);
    for (int src : filter) src_stat_highlight_.insert(src);
  }

  highlight_scale_ = params.find<double>("highlight_scale", 1000.);
  vtk_flicker_ = params.find<bool>("vtk_flicker", true);

}

SculpinSwitch::~SculpinSwitch()
{
  if (router_) delete router_;
}

void
SculpinSwitch::connectOutput(int src_outport, int dst_inport, EventLink::ptr&& link)
{
  double scale_factor = top_->portScaleFactor(my_addr_, src_outport);
  double port_bw = scale_factor * link_bw_;
  Port& p = ports_[src_outport];
  p.link = std::move(link);
  p.byte_delay = TimeDelta(1.0/port_bw);
  p.dst_port = dst_inport;
}

void
SculpinSwitch::connectInput(int /*src_outport*/, int /*dst_inport*/, 
                            EventLink::ptr&& /*link*/)
{
  //no-op
}

int
SculpinSwitch::queueLength(int port, int  /*vc*/) const
{
  auto& p = ports_[port];
  //VC basically ignored, all ports on "same" VC
  return p.priority_queue.size();
}

void
SculpinSwitch::handleCredit(Event * /*ev*/)
{
  spkt_abort_printf("SculpinSwitch::handleCredit: should never be called");
}

void
SculpinSwitch::send(Port& p, SculpinPacket* pkt, Timestamp now)
{
  if (now > p.next_free){
    p.next_free = now;
  }

  TimeDelta extra_delay = p.next_free - now;
  TimeDelta time_to_send = pkt->numBytes() * p.byte_delay;
  p.next_free += time_to_send;
  pkt->setTimeToSend(time_to_send);
  p.link->send(extra_delay, pkt);

#if SSTMAC_VTK_ENABLED
#if SSTMAC_INTEGRATED_SST_CORE
  traffic_event evt;
  evt.time_=p.next_free.ticks();
  evt.id_=my_addr_;
  evt.p_=p.id;
  evt.type_=1;
  traffic_intensity[p.id]->addData(evt);
#else
  if (vtk_){
    double scale = do_not_filter_packet(pkt);
    if (scale > 0){
      double color = delay.msec() * scale;
      vtk_->collect_new_color(now, p.id, color);
      if (vtk_flicker_ || p.priority_queue.empty()){
        static const timestamp flicker_diff(1e-9);
        timestamp flicker_time = p.next_free - flicker_diff;
        vtk_->collect_new_color(flicker_time, p.id, 0);
      }
    }
  }
#endif
#endif

  pkt_debug("packet leaving port %d at t=%8.4e: %s",
            p.id, p.next_free.sec(), pkt->toString().c_str());

  if (p.priority_queue.empty()){
    //reset the sequence number for ordering packets
    p.seqnum = 0;
  } else {
    pkt_debug("scheduling pull from port %d at t=%8.4e with %d queued",
              p.id, p.next_free.sec(), p.priority_queue.size());
    //schedule this port to pull another packet
    auto* ev = newCallback(this, &SculpinSwitch::pullNext, p.id);
    sendExecutionEvent(p.next_free, ev);
  }

}

void
SculpinSwitch::pullNext(int portnum)
{
  Port& p = ports_[portnum];
  if (p.priority_queue.empty()){
    spkt_abort_printf("SculpinSwitch::pull_next: incorrectly scheduled pull on switch %d from empty port %d",
                      int(addr()), p.id);
  }
  pkt_debug("pulling pending packet from port %d with %d queued", portnum, p.priority_queue.size());
  auto iter = p.priority_queue.begin();
  SculpinPacket* pkt = *iter;
  p.priority_queue.erase(iter);
  send(p, pkt, now());
}

void
SculpinSwitch::tryToSendPacket(SculpinPacket* pkt)
{
  Timestamp now_ = now();

  pkt->setArrival(now_);
  Port& p = ports_[pkt->nextPort()];
  pkt->setSeqnum(p.seqnum++);

  if (!congestion_){
    TimeDelta time_to_send = pkt->numBytes() * p.byte_delay;
    p.next_free += time_to_send;
    pkt->setTimeToSend(time_to_send);
    p.link->send(pkt);
  } else if (p.next_free > now_){
    //oh, well, I have to wait
    if (p.priority_queue.empty()){
      //nothing else is waiting - which means I have to schedule my own pull
      //schedule this port to pull another packet
      auto* ev = newCallback(this, &SculpinSwitch::pullNext, p.id);
      sendExecutionEvent(p.next_free, ev);
      pkt_debug("new packet has to schedule pull from port %d at t=%8.4e", p.id, p.next_free.sec());
    }
    pkt_debug("new packet has to wait on queue on port %d with %d queued", p.id, p.priority_queue.size());
    p.priority_queue.insert(pkt);
  } else if (p.priority_queue.empty()){
    //nothing there - go ahead and send
    send(p, pkt, now_);
  } else {
    //race condition - there is something in the queue
    //I must hop in the queue as well
    pkt_debug("new packet has to wait on queue on port %d with %d queued", p.id, p.priority_queue.size());
    p.priority_queue.insert(pkt);
  }
}

double
SculpinSwitch::doNotFilterPacket(SculpinPacket* pkt)
{
  auto iter = src_stat_highlight_.find(pkt->fromaddr());
  if (iter!= src_stat_highlight_.end()){
    return highlight_scale_;
  }
  iter = dst_stat_highlight_.find(pkt->toaddr());
  if (iter != dst_stat_highlight_.end()){
    return highlight_scale_;
  }


  bool keep_src = src_stat_filter_.empty() || src_stat_filter_.find(pkt->fromaddr()) != src_stat_filter_.end();
  bool keep_dst = dst_stat_filter_.empty() || dst_stat_filter_.find(pkt->fromaddr()) != dst_stat_filter_.end();

  if (keep_src && keep_dst) return 1.0;
  else return -1.;
}

void
SculpinSwitch::handlePayload(Event *ev)
{
  SculpinPacket* pkt = safe_cast(SculpinPacket, ev);
  switch_debug("handling payload %s", pkt->toString().c_str());
  router_->route(pkt);
  Port& p = ports_[pkt->nextPort()];

  TimeDelta time_to_send = p.byte_delay * pkt->numBytes();
  /** I am processing the head flit - so I assume compatibility with wormhole routing
   * The tail flit cannot leave THIS switch prior to its departure time in the prev switch */
  if (pkt->timeToSend() > time_to_send){
    //delay the packet
    auto ev = newCallback(this, &SculpinSwitch::tryToSendPacket, pkt);
    TimeDelta delta_t = pkt->timeToSend() - time_to_send;
    sendDelayedExecutionEvent(delta_t, ev);
  } else {
    tryToSendPacket(pkt);
  }
}

std::string
SculpinSwitch::toString() const
{
  return sprockit::printf("sculpin switch %d", int(my_addr_));
}

LinkHandler*
SculpinSwitch::creditHandler(int  /*port*/)
{
  return newLinkHandler(this, &SculpinSwitch::handleCredit);
}

LinkHandler*
SculpinSwitch::payloadHandler(int  /*port*/)
{
  return newLinkHandler(this, &SculpinSwitch::handlePayload);
}

}
}
