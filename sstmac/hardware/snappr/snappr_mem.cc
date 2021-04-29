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
#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif

#include <inttypes.h>
#include <sstmac/hardware/snappr/snappr_mem.h>
#include <sstmac/common/event_callback.h>

#define debug(str, ...) debug_printf(sprockit::dbg::memory, "Node %d: " str, addr(), __VA_ARGS__)

namespace sstmac {
namespace hw {

SnapprMemoryModel::SnapprMemoryModel(uint32_t id, SST::Params &params, Node* parent) :
  MemoryModel(id, params, parent),
  flowId_(0),
  channelInterleaver_(0) 
{
  flow_mtu_ = params.find<SST::UnitAlgebra>("flow_mtu", "512").getRoundedValue();

  mtu_ = params.find<SST::UnitAlgebra>("flow_mtu", "4096").getRoundedValue();

  auto max_bw = params.find<SST::UnitAlgebra>("channel_bandwidth");
  channel_byte_delay_ = TimeDelta(max_bw.getValue().inverse().toDouble());

  int num_channels = params.find<int>("num_channels");
  channels_.resize(num_channels);
  for (ChannelQueue& q : channels_){
    q.byte_delay = channel_byte_delay_;
  }

  flow_rsp_id_ = initialize( makeHandler(this, &SnapprMemoryModel::flowRequestResponse) );
}


void
SnapprMemoryModel::accessFlow(uint64_t bytes, TimeDelta byte_request_delay, Callback *cb)
{
  if (bytes == 0){
    sendExecutionEventNow(cb);
    return;
  }

  uint32_t flowId = flowId_++;
  debug("Starting flow of size %" PRIu64 " on ID %" PRIu32 " with request delay %10.5e",
        bytes, flowId, byte_request_delay.sec());
  uint32_t initial_bytes = bytes % flow_mtu_;
  if (initial_bytes == 0){
    initial_bytes = flow_mtu_;
  }

  FlowRequest* req = new FlowRequest;
  req->bytes = initial_bytes;
  req->flowId = flowId;
  auto* ev = newCallback(this, &SnapprMemoryModel::accessRequest, flow_rsp_id_, req);
  TimeDelta delay = byte_request_delay * initial_bytes;
  sendDelayedExecutionEvent(delay, ev);

  Flow& f = flows_[flowId];
  f.bytesLeft = bytes - initial_bytes;
  f.callback = cb;
  f.byteRequestDelay = byte_request_delay;
}

void
SnapprMemoryModel::flowRequestResponse(Request* req)
{
  FlowRequest* freq = static_cast<FlowRequest*>(req);
  Flow& f  = flows_[freq->flowId];
  if (f.bytesLeft == 0){
    debug("Receive %" PRIu32 " bytes of flow %" PRIu64 ": completing flow",
          req->bytes, freq->flowId);
    //this is a bit weird... we have to add the delay of the final packet
    //we are actually receiving the HEAD bit of the request right now
    sendDelayedExecutionEvent(req->bytes * channel_byte_delay_, f.callback);
    flows_.erase(freq->flowId);
    delete freq;
  } else {
    debug("Receive %" PRIu32 " bytes of flow %" PRIu64 ": %" PRIu64 " bytes left",
          req->bytes, freq->flowId, f.bytesLeft);
    f.bytesLeft -= flow_mtu_; //we have constructed so that the bytes left is always a multiple
    freq->bytes = flow_mtu_;
    auto* ev = newCallback(this, &SnapprMemoryModel::accessRequest, flow_rsp_id_, req);
    TimeDelta delay = flow_mtu_ * f.byteRequestDelay;
    sendDelayedExecutionEvent(delay, ev);
  }
}

void
SnapprMemoryModel::accessRequest(int linkId, Request *req)
{
  req->rspId = linkId;
  ChannelQueue& q = channels_[channelInterleaver_];
  q.next_free = std::max(q.next_free, now());
  TimeDelta timeToSend = q.byte_delay * req->bytes;
  //we send a response when the HEAD of the request is available
  sendExecutionEvent(q.next_free, newCallback(rsp_handlers_[linkId], &RequestHandlerBase::handle, req));
  q.next_free += timeToSend;
  debug("Channel %d busy with %d until %10.5e after delay of %10.5e",
        channelInterleaver_, linkId, q.next_free.sec(), timeToSend.sec());
  channelInterleaver_ = (channelInterleaver_ + 1) % channels_.size();
}

}
}
