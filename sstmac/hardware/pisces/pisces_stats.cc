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
#include <sstmac/hardware/pisces/pisces_stats.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/common/stats/stat_histogram.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>
#include <cinttypes>

MakeDebugSlot(pisces_stats)

#define debug(...) debug_printf(sprockit::dbg::pisces_stats, __VA_ARGS__)

namespace sstmac {
namespace hw {

RegisterNamespaces("bytes_sent", "congestion_spyplot", "congestion_delay",
                   "bytes_sent", "byte_hops", "delay_histogram");

static inline Timestamp
congestionDelay(const pkt_arbitration_t& st)
{
  Timestamp delta_t = st.tail_leaves - st.pkt->arrival();
  Timestamp min_delta_t = st.incoming_byte_delay * st.pkt->byteLength();
  if (delta_t > min_delta_t) return (delta_t - min_delta_t);
  else return Timestamp();
}

PacketStatsCallback::PacketStatsCallback(SST::Params& params, EventScheduler* parent)
{
}

void
PacketStatsCallback::collectFinalEvent(PiscesPacket *pkt)
{
  sprockit::abort("stats object does not support collecting final events");
}

void
PacketStatsCallback::collectSingleEvent(const pkt_arbitration_t &st)
{
  sprockit::abort("stats object does not support collecting single events");
}

CongestionSpyplot::CongestionSpyplot(SST::Params& params, EventScheduler* parent)
  : PacketStatsCallback(params, parent)
{
  //congestion_spyplot_ = requiredStats<StatSpyplot>(parent, params,
  //                                             "congestion_spyplot", "ascii");
}

CongestionSpyplot::~CongestionSpyplot()
{
  //these get delete by stats system
  //if (congestion_spyplot_) delete congestion_spyplot_;
}

void
CongestionSpyplot::collectFinalEvent(PiscesPacket* pkt)
{
  sprockit::abort("unimplemented: congestion_spyplot::collect_final_event");
  //double delay_ns = pkt->congestion_delay()*1e9;
  //congestion_spyplot_->add(pkt->fromaddr(), pkt->toaddr(), delay_ns);
}

void
CongestionSpyplot::collectSingleEvent(const pkt_arbitration_t &st)
{
  Timestamp delay = congestionDelay(st);
  collect(delay.sec(), st.pkt);
}

void
CongestionSpyplot::collect(double congestion_delay, PiscesPacket* pkt)
{
  congestion_spyplot_->addData(pkt->fromaddr(), pkt->toaddr(), congestion_delay);
}

DelayHistogram::DelayHistogram(SST::Params& params, EventScheduler* parent) :
  PacketStatsCallback(params, parent)
{
  //congestion_hist_ = requiredStats<StatHistogram>(parent, params,
  //                                      "delay_histogram", "histogram");
}

DelayHistogram::~DelayHistogram()
{
  //these get deleted by stats systems
  //if (congestion_hist_) delete congestion_hist_;
}

void
DelayHistogram::collectSingleEvent(const pkt_arbitration_t& st)
{
  Timestamp delay = congestionDelay(st);
  congestion_hist_->addData(delay.usec(), st.pkt->numBytes());
}

void
DelayHistogram::collectFinalEvent(PiscesPacket* pkt)
{
  sprockit::abort("unimplemented: delay_histogram::collect_final_event");
  //auto dpkt = safe_cast(pisces_delay_stats_packet, pkt);
  //debug("Accumulating final delay of %12.6e for packet on flow %" PRIu64,
  //      dpkt->congestion_delay(), dpkt->flow_id());
  //congestion_hist_->collect(dpkt->congestion_delay());
}


void
PacketDelayStats::collectSingleEvent(const pkt_arbitration_t& st)
{
  sprockit::abort("unimplemented: delay_histogram::collect_single_event");
  //double delay = congestion_delay(st);
  //auto dpkt = safe_cast(pisces_delay_stats_packet, st.pkt);
  //dpkt->accumulate_delay(delay);
}

MultiStats::MultiStats(SST::Params& params, EventScheduler *parent) :
  PacketStatsCallback(params, parent)
{
  std::vector<std::string> stats_list;
  params.find_array("callbacks", stats_list);
  cbacks_.reserve(stats_list.size());
  for (const std::string& str : stats_list){
    PacketStatsCallback* cb = PacketStatsCallback::factory::getValue(str, params, parent);
    cbacks_.push_back(cb);
  }
}

void
MultiStats::collectFinalEvent(PiscesPacket *pkt)
{
  for (auto cb : cbacks_){
    cb->collectFinalEvent(pkt);
  }
}

void
MultiStats::collectSingleEvent(const pkt_arbitration_t &st)
{
  for (auto cb : cbacks_){
    cb->collectSingleEvent(st);
  }
}

}
}
