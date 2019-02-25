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
  nchannels_ = params.find<int>("nchannels", 8);
  channels_available_.resize(nchannels_);
  for (int i=0; i < nchannels_; ++i){
    channels_available_[i] = i;
  }

  for (int i=0; i < nchannels_; ++i){
    channel_requests_.emplace_back(0,Timestamp(),nullptr);
  }

  packet_size_ = params.findUnits("mtu", "100GB").getRoundedValue();

  std::string max_bw_param = params.find<std::string>("total_bandwidth");
  SST::UnitAlgebra max_bw(max_bw_param);
  min_agg_byte_delay_ = Timestamp(max_bw.inverse().toDouble());
  min_flow_byte_delay_ =
      Timestamp(params.findUnits("max_single_bandwidth", max_bw_param).inverse().toDouble());
  latency_ = Timestamp(params.findUnits("latency").toDouble());
  arb_ = PiscesBandwidthArbitrator::create("macro", "cut_through", max_bw.toDouble());
}

PiscesMemoryModel::~PiscesMemoryModel()
{
  if (arb_) delete arb_;
}

void
PiscesMemoryModel::access(uint64_t bytes, Timestamp byte_delay, Callback* cb)
{
  if (channels_available_.empty()){
    stalled_requests_.emplace_back(bytes, byte_delay, cb);
  } else {
    int channel = channels_available_.back();
    channels_available_.pop_back();
    start(channel, bytes, byte_delay, cb);
  }
}

void
PiscesMemoryModel::start(int channel, uint64_t bytes, Timestamp byte_delay, Callback *cb)
{
  debug("Node %d starting access on channnel %d of size %ld with bw %8.4e",
        parent_node_->addr(), channel, bytes, 1.0/byte_delay.sec());

  if (bytes <= packet_size_){
    GlobalTimestamp t = access(channel, bytes, byte_delay, cb);
    sendExecutionEvent(t, newCallback(this, &PiscesMemoryModel::channelFree, channel));
  } else {
    Request& req = channel_requests_[channel];
    req.bytes_arrived = 0;
    req.bytes_total = bytes;
    req.cb = cb;
    req.byte_delay = byte_delay;
    access(channel, packet_size_, byte_delay,
           newCallback(this, &PiscesMemoryModel::dataArrived, channel, packet_size_));
  }

}

void
PiscesMemoryModel::channelFree(int channel)
{
  if (!stalled_requests_.empty()){
    Request& req = stalled_requests_.front();
    stalled_requests_.pop_front();
    start(channel, req.bytes_total, req.byte_delay, req.cb);
  } else {
    channels_available_.push_back(channel);
  }
}

void
PiscesMemoryModel::dataArrived(int channel, uint32_t bytes)
{
  Request& ch = channel_requests_[channel];
  ch.bytes_arrived += bytes;
  debug("Node %d channel %d now has %lu bytes arrived of %lu total",
        addr(), channel, ch.bytes_arrived, ch.bytes_total);
  if (ch.bytes_arrived == ch.bytes_total){
    ch.cb->execute();
    delete ch.cb;
    channelFree(channel);
  } else {
    uint32_t next_bytes = std::min(uint32_t(packet_size_),
                                   uint32_t(ch.bytes_total - ch.bytes_arrived));
    access(channel, next_bytes, ch.byte_delay,
           newCallback(this, &PiscesMemoryModel::dataArrived, channel, next_bytes));
  }
}

void
PiscesMemoryModel::access(PiscesPacket* pkt, Timestamp byte_delay, Callback* cb)
{
  if (channels_available_.empty()){
    stalled_requests_.emplace_back(byte_delay, cb, pkt);
  } else {
    int channel = channels_available_.back();
    channels_available_.pop_back();
    access(channel, pkt, byte_delay, cb);
  }
}

GlobalTimestamp
PiscesMemoryModel::access(int channel, PiscesPacket* pkt, Timestamp byte_delay, Callback* cb)
{
  pkt->setInport(channel);

  //set the bandwidth to the max single bw
  pkt->initByteDelay(byte_delay);
  pkt->setArrival(now());
  PiscesBandwidthArbitrator::IncomingPacket st;
  st.pkt = pkt;
  st.now = now();
  arb_->arbitrate(st);

  debug("Node %d memory packet %s leaving on channel %d at t=%8.4e",
    addr(), pkt->toString().c_str(), channel, st.tail_leaves.sec());

  //here we do not optimistically send credits = only when the packet leaves
  sendExecutionEvent(st.tail_leaves, cb);

  return st.tail_leaves;
}

GlobalTimestamp
PiscesMemoryModel::access(int channel, uint32_t bytes, Timestamp byte_delay, Callback* cb)
{
  PiscesPacket pkt(nullptr, bytes, -1, false, //doesn't matter
                    sstmac::NodeId(), sstmac::NodeId());
  GlobalTimestamp t = access(channel, &pkt, byte_delay, cb);
  return t;
}

}
}
