/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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
congestion_delay(const pkt_arbitration_t& st)
{
  timestamp delta_t = st.tail_leaves - st.pkt->arrival();
  double min_delta_t = st.pkt->byte_length() / st.incoming_bw;
  double congestion_delay = std::max(0., delta_t.sec() - min_delta_t);
  debug("Computed congestion delay of %12.6e for message with %ld "
       "bytes delta_t %12.6e and incoming bandwidth of %12.6e with "
       "min delta_t %12.6e",
       congestion_delay, st.pkt->byte_length(),
       delta_t, st.incoming_bw, min_delta_t);
  return congestion_delay;
}

packet_stats_callback::packet_stats_callback(sprockit::sim_parameters *params, event_scheduler* parent)
{
  id_ = params->get_int_param("id");
}

void
packet_stats_callback::collect_final_event(pisces_payload *pkt)
{
  spkt_throw(sprockit::value_error,
             "stats object does not support collecting final events");
}

void
packet_stats_callback::collect_single_event(const pkt_arbitration_t &st)
{
  spkt_throw(sprockit::value_error,
             "stats object does not support collecting single events");
}

congestion_spyplot::congestion_spyplot(sprockit::sim_parameters* params, event_scheduler* parent)
  : packet_stats_callback(params, parent)
{
  congestion_spyplot_ = required_stats<stat_spyplot>(parent, params,
                                               "congestion_spyplot", "ascii");
}

congestion_spyplot::~congestion_spyplot()
{
  if (congestion_spyplot_) delete congestion_spyplot_;
}

void
congestion_spyplot::collect_final_event(pisces_payload *pkt)
{
  auto dpkt = safe_cast(pisces_delay_stats_packet, pkt);
  double delay_ns = dpkt->congestion_delay()*1e9;
  congestion_spyplot_->add(pkt->fromaddr(), pkt->toaddr(), delay_ns);
}

void
congestion_spyplot::collect_single_event(const pkt_arbitration_t &st)
{
  double delay = congestion_delay(st);
  collect(delay, st.pkt);
}

void
congestion_spyplot::collect(double congestion_delay, pisces_payload* pkt)
{
  congestion_spyplot_->add(pkt->fromaddr(), pkt->toaddr(), congestion_delay);
}

delay_histogram::delay_histogram(sprockit::sim_parameters *params, event_scheduler* parent) :
  packet_stats_callback(params, parent)
{
  congestion_hist_ = required_stats<stat_histogram>(parent, params,
                                        "delay_histogram", "histogram");
}

delay_histogram::~delay_histogram()
{
  if (congestion_hist_) delete congestion_hist_;
}

void
delay_histogram::collect_single_event(const pkt_arbitration_t& st)
{
  double delay_us = congestion_delay(st);
  congestion_hist_->collect(delay_us);
}

void
delay_histogram::collect_final_event(pisces_payload* pkt)
{
  auto dpkt = safe_cast(pisces_delay_stats_packet, pkt);
  debug("Accumulating final delay of %12.6e for packet on flow %" PRIu64,
        dpkt->congestion_delay(), dpkt->flow_id());
  congestion_hist_->collect(dpkt->congestion_delay());
}


void
packet_delay_stats::collect_single_event(const pkt_arbitration_t& st)
{
  double delay = congestion_delay(st);
  auto dpkt = safe_cast(pisces_delay_stats_packet, st.pkt);
  dpkt->accumulate_delay(delay);
}

multi_stats::multi_stats(sprockit::sim_parameters *params, event_scheduler *parent) :
  packet_stats_callback(params, parent)
{
  std::vector<std::string> stats_list;
  params->get_vector_param("callbacks", stats_list);
  cbacks_.reserve(stats_list.size());
  for (const std::string& str : stats_list){
    packet_stats_callback* cb = packet_stats_callback::factory::get_value(str, params, parent);
    cbacks_.push_back(cb);
  }
}

void
multi_stats::collect_final_event(pisces_payload *pkt)
{
  for (auto cb : cbacks_){
    cb->collect_final_event(pkt);
  }
}

void
multi_stats::collect_single_event(const pkt_arbitration_t &st)
{
  for (auto cb : cbacks_){
    cb->collect_single_event(st);
  }
}

bytes_sent_collector::~bytes_sent_collector()
{
  if (bytes_sent_) delete bytes_sent_;
}

bytes_sent_collector::bytes_sent_collector(sprockit::sim_parameters *params,
                                           event_scheduler* parent) :
  packet_stats_callback(params, parent)
{
  bytes_sent_ = required_stats<stat_bytes_sent>(parent, params,
                                        "bytes_sent", "bytes_sent");
}

void
bytes_sent_collector::collect_single_event(const pkt_arbitration_t& st)
{
  bytes_sent_->record(st.pkt->next_port(), st.pkt->byte_length());
}

byte_hop_collector::~byte_hop_collector()
{
  delete byte_hops_;
}

byte_hop_collector::byte_hop_collector(sprockit::sim_parameters *params, event_scheduler* parent)
  : packet_stats_callback(params, parent)
{
  byte_hops_ = required_stats<stat_global_int>(parent, params,
                                        "byte_hops", "global_int");
  byte_hops_->set_label("Byte Hops");
}

void
byte_hop_collector::collect_single_event(const pkt_arbitration_t& st)
{
  byte_hops_->collect(st.pkt->byte_length());
}

void
stat_bytes_sent::output_switch(int sid, std::fstream& data_str)
{
  port_map& pmap = global_aggregation_[sid];
  port_map::iterator it, end = pmap.end();
  long total = 0;
  for (it=pmap.begin(); it != end; ++it){
    int port = it->first;
    long bytes = it->second;
    total += bytes;
    //coordinates neighbor_coords = top->neighbor_at_port(switch_id(sid), port);
    data_str << sprockit::printf("\t%3d %12ld\n", port, bytes);
  }
}

void
stat_bytes_sent::dump_local_data()
{
  spkt_throw(sprockit::unimplemented_error,
    "stat_bytes_sent::dump_local_data: makes no sense to dump local data");
}

void
stat_bytes_sent::dump_global_data()
{
  int num_switches = top_->num_switches();
  std::string data_file = fileroot_ + ".dat";
  std::fstream data_str;
  check_open(data_str, data_file);
  for (int i=0; i < num_switches; ++i){
    data_str << sprockit::printf("Switch %s\n", top_->label(device_id(i, device_id::router)).c_str());
    output_switch(i, data_str);
  }
  data_str.close();
}

void
stat_bytes_sent::global_reduce_non_root(parallel_runtime* rt, int root, char* buffer, int buffer_size)
{
  rt->send(root, buffer, buffer_size);
}

void
stat_bytes_sent::collect_buffer_at_root(char* buffer, int buffer_size)
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
stat_bytes_sent::collect_counts_at_root(parallel_runtime* rt, int src, global_gather_stats_t stats)
{
  char* recv_buffer = new char[stats.buffer_size];
  rt->recv(src, recv_buffer, stats.buffer_size);
  collect_buffer_at_root(recv_buffer, stats.buffer_size);
}

void
stat_bytes_sent::global_reduce_root(parallel_runtime* rt, global_gather_stats_t* stats,
                                    char* my_buffer, int my_buffer_size)
{
  //figure out how many total entries we have
  global_aggregation_.resize(top_->num_switches());

  for (int i=0; i < rt->nproc(); ++i){
    if (i != rt->me()){
      collect_counts_at_root(rt, i, stats[i]);
    } else {
      collect_buffer_at_root(my_buffer, my_buffer_size);
    }
  }
}

void
stat_bytes_sent::aggregation::entry::serialize_order(serializer& ser)
{
  ser & pmap;
  ser & sid;
}

stat_bytes_sent::stat_bytes_sent(sprockit::sim_parameters* params) :
    top_(nullptr),
    local_aggregation_(nullptr),
    stat_collector(params)
{
  top_ = sstmac::hw::topology::static_topology(params);
}

stat_bytes_sent::~stat_bytes_sent()
{
  if (local_aggregation_) delete local_aggregation_;
}

void
stat_bytes_sent::global_reduce(parallel_runtime *rt)
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
    global_reduce_root(rt, stats_array, buffer, total_size);
    delete[] stats_array;
  } else {
    global_reduce_non_root(rt, root, buffer, total_size);
  }

  delete[] buffer;
}

void
stat_bytes_sent::reduce(stat_collector *coll)
{
  stat_bytes_sent* input = safe_cast(stat_bytes_sent, coll);
  if (local_aggregation_ == nullptr){
    local_aggregation_ = new aggregation;
  }
  local_aggregation_->append(input->id(), input->port_map_);
}

void
stat_bytes_sent::simulation_finished(timestamp end)
{
  //no op
}

}
}