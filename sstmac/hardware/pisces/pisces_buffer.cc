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

#include <sstmac/hardware/pisces/pisces_buffer.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/common/runtime.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {


pisces_buffer::~pisces_buffer()
{
  if (input_.link) delete input_.link;
  if (output_.link) delete output_.link;
  if (arb_) delete arb_;
  if (payload_handler_) delete payload_handler_;
}

void
pisces_buffer::set_input(
  sprockit::sim_parameters* params,
  int this_inport, int src_outport,
  event_link* link)
{
  input_.link = link;
  input_.port_to_credit = src_outport;
  link->validate_latency(credit_lat_);
}

void
pisces_buffer::set_output(sprockit::sim_parameters* params,
                         int this_outport, int dst_inport,
                         event_link* link)
{
  link->validate_latency(send_lat_);
  output_.link = link;
  output_.arrival_port = dst_inport;
}

pisces_buffer::pisces_buffer(
  sprockit::sim_parameters* params,
  event_scheduler* parent, int num_vc)
  : pisces_sender(params, parent, false/*buffers do not update vc*/),
    bytes_delayed_(0),
    num_vc_(num_vc),
    queues_(num_vc),
    credits_(num_vc, 0),
 #if SSTMAC_SANITY_CHECK
    initial_credits_(num_vc,0),
 #endif
    packet_size_(params->get_byte_length_param("mtu")),
    payload_handler_(nullptr)
{
  int credits = params->get_byte_length_param("credits");
  long num_credits_per_vc = credits / num_vc_;
  for (int i=0; i < num_vc_; ++i) {
    credits_[i] = num_credits_per_vc;
#if SSTMAC_SANITY_CHECK
    initial_credits_[i] = num_credits_per_vc;
#endif
  }
  arb_ = pisces_bandwidth_arbitrator::factory::
          get_param("arbitrator", params);

  payload_handler_ = new_handler(this, &pisces_buffer::handle_payload);
}

void
pisces_buffer::handle_credit(event* ev)
{
  pisces_credit* credit = static_cast<pisces_credit*>(ev);
  int vc = credit->vc();
#if SSTMAC_SANITY_CHECK
  if (vc >= credits_.size()) {
    spkt_abort_printf("pisces_buffer::handle_credit: on %s, port %d, invalid vc %d",
                     to_string().c_str(), credit->port(), vc);
  }
#endif
  int& num_credits = credits_[vc];
  num_credits += credit->num_credits();
  //we've cleared out some of the delay
  bytes_delayed_ -= credit->num_credits();

  pisces_debug(
    "On %s with %d credits, handling credit {%s} for vc:%d -> byte delay now %d",
     to_string().c_str(),
     num_credits,
     credit->to_string().c_str(),
     vc, bytes_delayed_);

#if SSTMAC_SANITY_CHECK
  if (credit->port() != 0){
    spkt_abort_printf("pisces_buffer::handle_credit: got nonzero port");
  }

  if (num_credits > initial_credits_[vc]){
    spkt_abort_printf("initial credits exceeded");
  }
#endif

  /** while we have sendable payloads, do it */
  pisces_packet* payload = queues_[vc].pop(num_credits);
  while (payload) {
    num_credits -= payload->num_bytes();
    //this actually doesn't create any new delay
    //this message was already queued so num_bytes
    //was already added to bytes_delayed
    send(arb_, payload, input_, output_);
    payload = queues_[vc].pop(num_credits);
  }

  delete credit;
}

void
pisces_buffer::handle_payload(event* ev)
{
  auto pkt = static_cast<pisces_packet*>(ev);
  pkt->set_arrival(now());
  int dst_vc = pkt->vc();
#if SSTMAC_SANITY_CHECK
  //vc default to uninit instead of zero to make sure routers set VC
  dst_vc = dst_vc == routing::uninitialized ? 0 : dst_vc;
#endif

#if SSTMAC_SANITY_CHECK
  if (dst_vc >= credits_.size()) {
    spkt_abort_printf("pisces_buffer::handle_payload: on %s, port %d, invalid vc %d",
                     to_string().c_str(), pkt->edge_outport(), dst_vc);
  }
#endif

  int& num_credits = credits_[dst_vc];
  pisces_debug(
    "On %s with %d credits, handling payload {%s} for vc:%d",
    to_string().c_str(), num_credits,
    pkt->to_string().c_str(), dst_vc);

  // it either gets queued or gets sent
  // either way there's a delay accumulating for other messages
  bytes_delayed_ += pkt->num_bytes();
  if (num_credits >= pkt->num_bytes()) {
    num_credits -= pkt->num_bytes();
    send(arb_, pkt, input_, output_);
  } else {
#if SSTMAC_SANITY_CHECK
    if (dst_vc >= queues_.size()){
      spkt_abort_printf("Bad VC %d: max is %d", dst_vc, queues_.size() - 1);
    }
#endif
    queues_[dst_vc].push_back(pkt);
  }
}

void
pisces_buffer::deadlock_check()
{
#if !SSTMAC_INTEGRATED_SST_CORE
  for (int i=0; i < queues_.size(); ++i){
    payload_queue& queue = queues_[i];
    pisces_packet* pkt = queue.front();
    if (pkt){
      int vc = pkt->next_vc();
      deadlocked_channels_.insert(vc);
      vc = update_vc_ ? pkt->next_vc() : pkt->vc();
      std::cerr << "Starting deadlock check on " << to_string() << " on queue " << i
        << " going to " << output_.link->to_string()
        << " outport=" << pkt->edge_outport()
        << " vc=" << vc
        << std::endl;
      output_.link->deadlock_check(pkt);
    }
  }
#endif
}

void
pisces_buffer::build_blocked_messages()
{
  //std::cerr << "\tbuild blocked messages on " << to_string() << std::endl;
  for (int i=0; i < queues_.size(); ++i){
    payload_queue& queue = queues_[i];
    pisces_packet* pkt = queue.pop(1000000);
    while (pkt){
      blocked_messages_[pkt->vc()].push_back(pkt);
      //std::cerr << "\t\t" << "into port=" << msg->inport() << " vc=" << msg->vc()
      //  << " out on port=" << msg->port() << " vc=" << msg->routable_message::vc() << std::endl;
      pkt = queue.pop(10000000);
    }
  }
}

void
pisces_buffer::deadlock_check(event* ev)
{
#if !SSTMAC_INTEGRATED_SST_CORE
  if (blocked_messages_.empty()){
    build_blocked_messages();
  }

  pisces_packet* payload = safe_cast(pisces_packet, ev);
  int vc = update_vc_ ? payload->next_vc() : payload->vc();
  if (deadlocked_channels_.find(vc) != deadlocked_channels_.end()){
    spkt_throw_printf(sprockit::value_error,
      "found deadlock:\n%s", to_string().c_str());
  }

  deadlocked_channels_.insert(vc);

  std::list<pisces_packet*>& blocked = blocked_messages_[vc];
  if (blocked.empty()){
    int outport = payload->next_local_outport();
    int inport = payload->next_local_inport();
    spkt_abort_printf("channel is NOT blocked on deadlock check on outport=%d inport=%d vc=%d",
      outport, inport, vc);
  } else {
    pisces_packet* next = blocked.front();
    std::cerr << to_string() << " going to "
      << output_.link->to_string()
      << " outport=" << next->edge_outport()
      << " vc=" << next->next_vc()
      << " : " << next->to_string()
      << std::endl;
    output_.link->deadlock_check(next);
  }
#endif
}

int
pisces_buffer::queue_length() const
{
  uint32_t bytes_sending = arb_->bytes_sending(now());
  uint32_t total_bytes_pending = bytes_sending + bytes_delayed_;
  int queue_length = total_bytes_pending / packet_size_;
  debug_printf(sprockit::dbg::pisces | sprockit::dbg::pisces_queue,
    "On %s, %u bytes delayed, %u bytes sending, %d total pending, %d packets in queue",
     to_string().c_str(),
     bytes_delayed_,
     bytes_sending,
     total_bytes_pending, queue_length);
  return std::max(0, queue_length);
}

pisces_endpoint::pisces_endpoint(sprockit::sim_parameters *params, event_scheduler *parent,
                                 event_handler* handler) :
  pisces_sender(params, parent, false/*no vc update on buffer*/),
  output_handler_(handler)
{
}

pisces_endpoint::~pisces_endpoint()
{
  if (output_handler_) delete output_handler_;
}

void
pisces_endpoint::handle_payload(event* ev)
{
  auto pkt = static_cast<pisces_packet*>(ev);
  pkt->set_arrival(now());
  debug_printf(sprockit::dbg::pisces,
    "On %s, handling payload {%s}, done sending",
    to_string().c_str(),
    pkt->to_string().c_str());

  output_handler_->handle(pkt);

  //endpoints are assumed to be infinite
  //immediately return a credit
  send_credit(input_, pkt, now());
}

void
pisces_endpoint::handle_credit(event* ev)
{
  spkt_throw_printf(sprockit::illformed_error,
                   "pisces_eject_buffer::handle_credit: should not handle credits");
}

void
pisces_endpoint::set_output(sprockit::sim_parameters *params, int this_outport, int dst_inport, event_link *link)
{
  spkt_abort_printf("pisces_endpoint::set_output: should not be called, endpoint has no output");
}


}
}
