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

std::string
memory_message::to_string() const
{
  uint32_t num, node;
  unique_event_id::unpack(id_, node, num);
  return sprockit::printf("memory message %lu: seqnum %d on node %d with %d bytes",
                          id_, node, num, bytes_);
}

pisces_memory_packetizer::pisces_memory_packetizer(
  sprockit::sim_parameters* params,
  event_scheduler* parent) :
  arb_(nullptr),
  bw_noise_(nullptr),
  interval_noise_(nullptr),
  num_noisy_intervals_(0),
  packetizer(params, parent)
{
  for (int i=0; i < PISCES_MEM_DEFAULT_NUM_CHANNELS; ++i){
    channelFree_[i] = true;
  }

  if (!params->has_param("mtu"))
    params->add_param("mtu", "100GB");

  max_bw_ = params->get_bandwidth_param("total_bandwidth");
  max_single_bw_ = params->get_optional_bandwidth_param("max_single_bandwidth", max_bw_);
  latency_ = params->get_time_param("latency");
  arb_ = pisces_bandwidth_arbitrator::factory::get_value("cut_through", params);

  init_noise_model();

  debug("initializing pisces memory packetizer with mtu %d", packetSize());
}

link_handler*
pisces_memory_packetizer::new_credit_handler() const
{
  spkt_abort_printf("pisces_memory_packetizer::new_ack_handler: not used");
  return nullptr;
}

link_handler*
pisces_memory_packetizer::new_payload_handler() const
{
  spkt_abort_printf("pisces_memory_packetizer::new_payload_handler: not used");
  return nullptr;
}

pisces_memory_packetizer::~pisces_memory_packetizer()
{
  if (arb_) delete arb_;
  if (bw_noise_) delete bw_noise_;
  if (interval_noise_) delete interval_noise_;
}

pisces_memory_model::pisces_memory_model(sprockit::sim_parameters *params, node *nd) :
  memory_model(params, nd)
{
  nchannels_ = PISCES_MEM_DEFAULT_NUM_CHANNELS;
  channels_available_.resize(nchannels_);
  for (int i=0; i < nchannels_; ++i){
    channels_available_[i] = i;
  }

  mem_packetizer_ = new pisces_memory_packetizer(params, nd);
  mem_packetizer_->setArrivalNotify(this);
}

pisces_memory_model::~pisces_memory_model()
{
  if (mem_packetizer_) delete mem_packetizer_;
}

void
pisces_memory_model::access(
  long bytes, double max_bw,
  callback* cb)
{
  memory_message* msg = new memory_message(bytes,
                   parent_node_->allocate_unique_id(), max_bw);

  if (channels_available_.empty()){
    stalled_requests_.push_back(std::make_pair(msg,cb));
  } else {
    int channel = channels_available_.back();
    channels_available_.pop_back();
    start(channel, msg, cb);
  }
}

void
pisces_memory_model::start(int channel, memory_message* msg, callback *cb)
{
  debug("Node %d starting access %lu on vn %d of size %ld with bw %8.4e",
        parent_node_->addr(), msg->flow_id(), channel, msg->byte_length(), msg->max_bw());
  mem_packetizer_->start(channel, msg);
  pending_requests_[msg] = cb;
}

void
pisces_memory_model::notify(int vn, message* msg)
{
  debug("Node %d finished access %lu on vn %d of size %ld",
        parent_node_->addr(), msg->flow_id(), vn, msg->byte_length());

  callback* cb = pending_requests_[msg];
  pending_requests_.erase(msg);
  //happening now
  delete msg;
  send_now_self_event_queue(cb);
  if (stalled_requests_.empty()){
    channels_available_.push_back(vn);
  } else {
    auto& pair = stalled_requests_.front();
    start(vn, pair.first, pair.second);
    stalled_requests_.pop_front();
  }
}

#if 0
int
pisces_memory_model::allocate_channel()
{
  if (channels_available_.empty()){
    //double size of pending
    int newsize = nchannels_*2;
    for (int i=nchannels_; i != newsize; ++i){
      channels_available_.push_back(i);
    }
    nchannels_ = newsize;
  }
  int channel = channels_available_.front();
  channels_available_.pop_front();
  return channel;
}
#endif

void
pisces_memory_packetizer::init_noise_model()
{
  if (bw_noise_){
    arb_->partition(interval_noise_, num_noisy_intervals_);
    arb_->init_noise_model(bw_noise_);
  }
}

void
pisces_memory_packetizer::inject(int vn, uint32_t bytes, uint64_t byte_offset, message* msg)
{
  bool is_tail = (bytes + byte_offset) == msg->byte_length();
  pisces_payload* payload = new pisces_payload(is_tail ? msg : nullptr,
                                               bytes, msg->flow_id(), is_tail,
                                               msg->fromaddr(), msg->toaddr());

  payload->set_inport(vn);
  memory_message* orig = safe_cast(memory_message, msg);
  if (orig->max_bw() != 0){
    payload->set_bw(orig->max_bw());
  }

  debug("injecting %s on vn %d of size=%ld bw=%8.4e",
        payload->to_string().c_str(), vn, payload->byte_length(), payload->bw());

  channelFree_[vn] = false;
  handle_payload(vn, payload);
}

void
pisces_memory_packetizer::handle_payload(int vn, pisces_payload* pkt)
{
  //set the bandwidth to the max single bw
  pkt->init_bw(max_single_bw_);
  pkt->set_arrival(now().sec());
  pkt_arbitration_t st;
  st.pkt = pkt;
  st.now = now();
  arb_->arbitrate(st);

  debug("memory packet %s leaving on vn %d at t=%8.4e",
    pkt->to_string().c_str(), vn, st.tail_leaves.sec());

  send_self_event_queue(st.tail_leaves,
    new_callback(this, &packetizer::packetArrived, vn, pkt));

  //send some credits back
  int ignore_vc = -1;
  pisces_credit* credit = new pisces_credit(vn, ignore_vc, pkt->num_bytes());
  //here we do not optimistically send credits = only when the packet leaves
  send_self_event_queue(st.tail_leaves,
    new_callback(this, &pisces_memory_packetizer::recv_credit, credit));
}

void
pisces_memory_packetizer::recv_credit(event* ev)
{
  pisces_credit* credit = static_cast<pisces_credit*>(ev);
  debug("got credit %s on vn %d", credit->to_string().c_str(), credit->port());

  int channel = credit->port();
  delete credit;
  channelFree_[channel] = true;
  sendWhatYouCan(channel);
}

}
}
