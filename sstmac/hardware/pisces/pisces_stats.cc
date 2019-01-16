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
#include <sstmac/common/stats/stat_global_int.h>
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

static inline double
congestionDelay(const pkt_arbitration_t& st)
{
  Timestamp delta_t = st.tail_leaves - st.pkt->arrival();
  double min_delta_t = st.pkt->byteLength() / st.incoming_bw;
  double congestion_delay = std::max(0., delta_t.sec() - min_delta_t);
  debug("Computed congestion delay of %12.6e for message with %ld "
       "bytes delta_t %12.6e and incoming bandwidth of %12.6e with "
       "min delta_t %12.6e",
       congestion_delay, st.pkt->byteLength(),
       delta_t, st.incoming_bw, min_delta_t);
  return congestion_delay;
}

PacketStatsCallback::PacketStatsCallback(SST::Params& params, EventScheduler* parent)
{
  id_ = params->get_int_param("id");
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
  double delay = congestionDelay(st);
  collect(delay, st.pkt);
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
  double delay_us = congestionDelay(st);
  congestion_hist_->addData(delay_us);
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
  params->get_vector_param("callbacks", stats_list);
  cbacks_.reserve(stats_list.size());
  for (const std::string& str : stats_list){
    PacketStatsCallback* cb = PacketStatsCallback::factory::get_value(str, params, parent);
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

BytesSentCollector::~BytesSentCollector()
{
  //if (bytes_sent_) delete bytes_sent_;
}

BytesSentCollector::BytesSentCollector(SST::Params& params,
                                       EventScheduler* parent) :
  PacketStatsCallback(params, parent)
{
  /** TODO stats
  bytes_sent_ = requiredStats<StatBytesSent>(parent, params,
                                        "bytes_sent", "bytes_sent");
  */
}

void
BytesSentCollector::collectSingleEvent(const pkt_arbitration_t& st)
{
  bytes_sent_->addData(st.pkt->edgeOutport(), st.pkt->byteLength());
}

ByteHopCollector::~ByteHopCollector()
{
  //delete byte_hops_;
}

ByteHopCollector::ByteHopCollector(SST::Params& params, EventScheduler* parent)
  : PacketStatsCallback(params, parent)
{
  /** TODO stats
  byte_hops_ = requiredStats<StatGlobalInt>(parent, params,
                                        "byte_hops", "global_int");
  byte_hops_->setLabel("Byte Hops");
  */
}

void
ByteHopCollector::collectSingleEvent(const pkt_arbitration_t& st)
{
  /** TODO stats
  byte_hops_->collect(st.pkt->byteLength());
  */
}

void
StatBytesSent::outputSwitch(int sid, std::fstream& data_str)
{
  port_map& pmap = global_aggregation_[sid];
  port_map::iterator it, end = pmap.end();
  long total = 0;
  for (it=pmap.begin(); it != end; ++it){
    int port = it->first;
    long bytes = it->second;
    total += bytes;
    //coordinates neighbor_coords = top->neighborAtPort(SwitchId(sid), port);
    data_str << sprockit::printf("\t%3d %12ld\n", port, bytes);
  }
}

void
StatBytesSent::globalReduceNonRoot(ParallelRuntime* rt, int root, char* buffer, int buffer_size)
{
  rt->send(root, buffer, buffer_size);
}

void
StatBytesSent::collectbufferAtRoot(char* buffer, int buffer_size)
{
  serializer ser;
  ser.start_unpacking(buffer, buffer_size);

  std::vector<aggregation::entry> entries;
  ser & entries;

  int num_entries = entries.size();
  for (int i=0; i < num_entries; ++i){
    aggregation::entry& entry = entries[i];
    global_aggregation_[entry.sid] = entry.pmap;
  }
}

void
StatBytesSent::collectCountsAtRoot(ParallelRuntime* rt, int src, global_gather_stats_t stats)
{
  char* recv_buffer = new char[stats.buffer_size];
  rt->recv(src, recv_buffer, stats.buffer_size);
  collectbufferAtRoot(recv_buffer, stats.buffer_size);
}

void
StatBytesSent::globalReduceRoot(ParallelRuntime* rt, global_gather_stats_t* stats,
                                    char* my_buffer, int my_buffer_size)
{
  //figure out how many total entries we have
  global_aggregation_.resize(top_->numSwitches());

  for (int i=0; i < rt->nproc(); ++i){
    if (i != rt->me()){
      collectCountsAtRoot(rt, i, stats[i]);
    } else {
      collectbufferAtRoot(my_buffer, my_buffer_size);
    }
  }
}

void
StatBytesSent::aggregation::entry::serialize_order(serializer& ser)
{
  ser & pmap;
  ser & sid;
}

StatBytesSent::StatBytesSent(SST::Params& params) :
    top_(nullptr),
    local_aggregation_(nullptr),
    Parent(params)
{
  top_ = sstmac::hw::Topology::staticTopology(params);
}

StatBytesSent::~StatBytesSent()
{
  if (local_aggregation_) delete local_aggregation_;
}

/** TODO stats
void
StatBytesSent::globalReduce(ParallelRuntime *rt)
{
  //determine how big of an array we we will need to receive from each person
  int root = 0;

  serializer ser;
  ser.start_sizing();
  //this is basically a sanity check
  ser & local_aggregation_->entries_;
  int total_size = ser.size();

  int size_check = local_aggregation_->ser_size();
  if (total_size != size_check){
    spkt_throw_printf(sprockit::value_error,
        "stat_bytes_sent::global_reduce: serialization buffer size %d does not match expected %d - %d entries and %d counts",
         total_size, size_check, local_aggregation_->num_entries(), local_aggregation_->num_counts());
  }

  char* buffer = new char[total_size];
  ser.start_packing(buffer, total_size);
  ser & local_aggregation_->entries_;

  global_gather_stats_t stats;
  stats.buffer_size = total_size;
  global_gather_stats_t* stats_array = nullptr;

  if (rt->me() == root){
    stats_array = new global_gather_stats_t[rt->nproc()];
  }

  rt->gather(&stats, sizeof(global_gather_stats_t), stats_array, root);

  if (rt->me() == root){
    globalReduceRoot(rt, stats_array, buffer, total_size);
    delete[] stats_array;
  } else {
    globalReduceNonRoot(rt, root, buffer, total_size);
  }

  delete[] buffer;
}

void
StatBytesSent::reduce(StatCollector *coll)
{
  StatBytesSent* input = safe_cast(StatBytesSent, coll);
  if (local_aggregation_ == nullptr){
    local_aggregation_ = new aggregation;
  }
  local_aggregation_->append(input->id(), input->port_map_);
}
*/

}
}
