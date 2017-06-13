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

#include <sstmac/hardware/pisces/pisces_memory_model.h>
#include <sstmac/hardware/pisces/packet_allocator.h>
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
"total_bandwidth",
"max_single_bandwidth",
"pisces_memory_mtu",
"pisces_memory_latency",
"pisces_memory_bandwidth",
"pisces_memory_single_bandwidth",
"pisces_memory_arbitrator",
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
  if (!params->has_param("mtu"))
    params->add_param("mtu", "100GB");

  max_bw_ = params->get_bandwidth_param("total_bandwidth");
  max_single_bw_ = params->get_optional_bandwidth_param("max_single_bandwidth", max_bw_);
  latency_ = params->get_time_param("latency");
  arb_ = pisces_bandwidth_arbitrator::factory::get_value("cut_through", params);
  pkt_allocator_ = packet_allocator::factory
      ::get_optional_param("packet_allocator", "pisces", params);

  init_noise_model();

  self_credit_handler_ = new_handler(this, &pisces_memory_packetizer::recv_credit);

  debug("initializing pisces memory packetizer with mtu %d", packetSize());
}

link_handler*
pisces_memory_packetizer::new_credit_handler() const
{
  spkt_abort_printf("pisces_memory_packetizer::new_ack_handler: not used");
}

link_handler*
pisces_memory_packetizer::new_payload_handler() const
{
  spkt_abort_printf("pisces_memory_packetizer::new_payload_handler: not used");
}

pisces_memory_packetizer::~pisces_memory_packetizer()
{
  if (arb_) delete arb_;
  if (pkt_allocator_) delete pkt_allocator_;
  if (bw_noise_) delete bw_noise_;
  if (interval_noise_) delete interval_noise_;
  if (self_credit_handler_) delete self_credit_handler_;
}

pisces_memory_model::pisces_memory_model(sprockit::sim_parameters *params, node *nd) :
  memory_model(params, nd)
{
  nchannels_ = 4;
  for (int i=0; i < nchannels_; ++i){
    channels_available_.push_back(i);
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
  pending_requests_[msg] = cb;
  int channel = allocate_channel();
  debug("Node %d starting access %lu on vn %d",
        parent_node_->addr(), msg->flow_id(), channel);
  mem_packetizer_->start(channel, msg);
}

void
pisces_memory_model::notify(int vn, message* msg)
{
  debug("finished access %lu on vn %d", msg->flow_id(), vn);

  callback* cb = pending_requests_[msg];
  pending_requests_.erase(msg);
  channels_available_.push_front(vn);
  //happening now
  delete msg;
  schedule_now(cb);
}

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

void
pisces_memory_packetizer::init_noise_model()
{
  if (bw_noise_){
    arb_->partition(interval_noise_, num_noisy_intervals_);
    arb_->init_noise_model(bw_noise_);
  }
}

void
pisces_memory_packetizer::inject(int vn, long bytes, long byte_offset, message* msg)
{
  bool is_tail = (bytes + byte_offset) == msg->byte_length();
  pisces_payload* payload = pkt_allocator_->new_packet(bytes, msg->flow_id(), is_tail,
                                                       msg->toaddr(), msg->fromaddr(),
                                                       is_tail ? msg : nullptr);

  payload->set_inport(vn);
  memory_message* orig = safe_cast(memory_message, msg);
  if (orig->max_bw() != 0){
    payload->set_bw(orig->max_bw());
  }

  debug("injecting %s on vn %d", payload->to_string().c_str(), vn);

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

  //might need to send some credits back
  if (!pkt->is_tail()){
    int ignore_vc = -1;
    pisces_credit* credit = new pisces_credit(vn, ignore_vc, pkt->num_bytes());
    //here we do not optimistically send credits = only when the packet leaves
    schedule(st.tail_leaves, self_credit_handler_, credit);
  }
}

void
pisces_memory_packetizer::recv_credit(event* ev)
{
  pisces_credit* credit = static_cast<pisces_credit*>(ev);
  debug("got credit %s on vn %d", credit->to_string().c_str(), credit->port());

  int channel = credit->port();
  delete credit;
  sendWhatYouCan(channel);
}

}
}