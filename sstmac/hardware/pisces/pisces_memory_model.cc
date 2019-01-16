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

#include <sstmac/hardware/pisces/pisces_memory_model.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/event_callback.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>

MakeDebugSlot(pisces_memory)
RegisterKeywords(
{ "total_bandwidth", "the total, aggregate bandwidth of the memory" },
{ "max_single_bandwidth", "the max bandwidth of a single memory stream" },
{ "latency", "the latency of a single memory operations" },
);

#define debug(...) debug_printf(sprockit::dbg::pisces_memory, __VA_ARGS__)

namespace sstmac {
namespace hw {




PiscesMemoryModel::PiscesMemoryModel(SST::Params& params, Node *nd) :
  arb_(nullptr),
  MemoryModel(params, nd)
{
  nchannels_ = params->get_optional_int_param("nchannels", 8);
  channels_available_.resize(nchannels_);
  for (int i=0; i < nchannels_; ++i){
    channels_available_[i] = i;
  }

  for (int i=0; i < nchannels_; ++i){
    channel_requests_.emplace_back(0,0,nullptr);
  }

  packet_size_ = params->get_optional_byte_length_param("mtu", 100e9);
  max_bw_ = params->get_bandwidth_param("total_bandwidth");
  max_single_bw_ = params->get_optional_bandwidth_param("max_single_bandwidth", max_bw_);
  latency_ = params->get_time_param("latency");
  arb_ = PiscesBandwidthArbitrator::factory::get_value("cut_through", params);
}

PiscesMemoryModel::~PiscesMemoryModel()
{
  if (arb_) delete arb_;
}

void
PiscesMemoryModel::access(uint64_t bytes, double max_bw, Callback* cb)
{
  if (channels_available_.empty()){
    stalled_requests_.emplace_back(bytes, max_bw, cb);
  } else {
    int channel = channels_available_.back();
    channels_available_.pop_back();
    start(channel, bytes, max_bw, cb);
  }
}

void
PiscesMemoryModel::start(int channel, uint64_t bytes, double max_bw, Callback *cb)
{
  debug("Node %d starting access on channnel %d of size %ld with bw %8.4e",
        parent_node_->addr(), channel, bytes, max_single_bw_);

  if (bytes <= packet_size_){
    Timestamp t = access(channel, bytes, max_bw, cb);
    sendExecutionEvent(t, newCallback(this, &PiscesMemoryModel::channel_free, channel));
  } else {
    request& req = channel_requests_[channel];
    req.bytes_arrived = 0;
    req.bytes_total = bytes;
    req.cb = cb;
    req.max_bw = max_bw;
    access(channel, packet_size_, max_bw,
           newCallback(this, &PiscesMemoryModel::data_arrived, channel, packet_size_));
  }

}

void
PiscesMemoryModel::channel_free(int channel)
{
  if (!stalled_requests_.empty()){
    request& req = stalled_requests_.front();
    stalled_requests_.pop_front();
    start(channel, req.bytes_total, req.max_bw, req.cb);
  } else {
    channels_available_.push_back(channel);
  }
}

void
PiscesMemoryModel::data_arrived(int channel, uint32_t bytes)
{
  request& ch = channel_requests_[channel];
  ch.bytes_arrived += bytes;
  debug("Node %d channel %d now has %lu bytes arrived of %lu total",
        addr(), channel, ch.bytes_arrived, ch.bytes_total);
  if (ch.bytes_arrived == ch.bytes_total){
    ch.cb->execute();
    delete ch.cb;
    channel_free(channel);
  } else {
    uint32_t next_bytes = std::min(uint32_t(packet_size_),
                                   uint32_t(ch.bytes_total - ch.bytes_arrived));
    access(channel, next_bytes, ch.max_bw,
           newCallback(this, &PiscesMemoryModel::data_arrived, channel, next_bytes));
  }
}

void
PiscesMemoryModel::access(PiscesPacket* pkt, double max_bw, Callback* cb)
{
  if (channels_available_.empty()){
    stalled_requests_.emplace_back(max_bw, cb, pkt);
  } else {
    int channel = channels_available_.back();
    channels_available_.pop_back();
    access(channel, pkt, max_bw, cb);
  }
}

Timestamp
PiscesMemoryModel::access(int channel, PiscesPacket* pkt, double max_bw, Callback* cb)
{
  pkt->setInport(channel);

  //set the bandwidth to the max single bw
  pkt->initBw(max_bw);
  pkt->setArrival(now().sec());
  pkt_arbitration_t st;
  st.pkt = pkt;
  st.now = now();
  arb_->arbitrate(st);

  debug("Node %d memory packet %s leaving on channel %d at t=%8.4e",
    addr(), pkt->toString().c_str(), channel, st.tail_leaves.sec());

  //here we do not optimistically send credits = only when the packet leaves
  sendExecutionEvent(st.tail_leaves, cb);

  return st.tail_leaves;
}

Timestamp
PiscesMemoryModel::access(int channel, uint32_t bytes, double max_bw, Callback* cb)
{
  PiscesPacket pkt(nullptr, bytes, -1, false, //doesn't matter
                    sstmac::NodeId(), sstmac::NodeId());
  Timestamp t = access(channel, &pkt, max_bw, cb);
  return t;
}

}
}
