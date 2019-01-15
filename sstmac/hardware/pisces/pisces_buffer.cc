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


PiscesBuffer::~PiscesBuffer()
{
  if (input_.link) delete input_.link;
  if (output_.link) delete output_.link;
  if (arb_) delete arb_;
}

void
PiscesBuffer::setInput(
  sprockit::sim_parameters::ptr& params,
  int this_inport, int src_outport,
  EventLink* link)
{
  input_.link = link;
  input_.port_to_credit = src_outport;
}

void
PiscesBuffer::setOutput(sprockit::sim_parameters::ptr& params,
                         int this_outport, int dst_inport,
                         EventLink* link)
{
  output_.link = link;
  output_.arrival_port = dst_inport;
}

PiscesBuffer::PiscesBuffer(
  sprockit::sim_parameters::ptr& params,
  SST::Component* parent, int num_vc)
  : PiscesSender(params, parent, false/*buffers do not update vc*/),
    bytes_delayed_(0),
    num_vc_(num_vc),
    queues_(num_vc),
    credits_(num_vc, 0),
 #if SSTMAC_SANITY_CHECK
    initial_credits_(num_vc,0),
 #endif
    packet_size_(params->get_byte_length_param("mtu"))
{
  int credits = params->get_byte_length_param("credits");
  int num_credits_per_vc = credits / num_vc_;
  for (int i=0; i < num_vc_; ++i) {
    credits_[i] = num_credits_per_vc;
#if SSTMAC_SANITY_CHECK
    initial_credits_[i] = num_credits_per_vc;
#endif
  }
  arb_ = PiscesBandwidthArbitrator::factory::
          get_param("arbitrator", params);
}

void
PiscesBuffer::handleCredit(Event* ev)
{
  PiscesCredit* credit = static_cast<PiscesCredit*>(ev);
  int vc = credit->vc();
#if SSTMAC_SANITY_CHECK
  if (vc >= credits_.size()) {
    spkt_abort_printf("pisces_buffer::handleCredit: on %s, port %d, invalid vc %d",
                     toString().c_str(), credit->port(), vc);
  }
#endif
  int& num_credits = credits_[vc];
  num_credits += credit->numCredits();
  //we've cleared out some of the delay
  bytes_delayed_ -= credit->numCredits();

  pisces_debug(
    "On %s with %d credits, handling credit {%s} for vc:%d -> byte delay now %d",
     toString().c_str(),
     num_credits,
     credit->toString().c_str(),
     vc, bytes_delayed_);

#if SSTMAC_SANITY_CHECK
  if (credit->port() != 0){
    spkt_abort_printf("pisces_buffer::handleCredit: got nonzero port");
  }

  if (num_credits > initial_credits_[vc]){
    spkt_abort_printf("initial credits exceeded on %s",
                      toString().c_str());
  }
#endif

  /** while we have sendable payloads, do it */
  PiscesPacket* payload = queues_[vc].pop(num_credits);
  while (payload) {
    num_credits -= payload->numBytes();
    //this actually doesn't create any new delay
    //this message was already queued so num_bytes
    //was already added to bytes_delayed
    send(arb_, payload, input_, output_);
    payload = queues_[vc].pop(num_credits);
  }

  delete credit;
}

void
PiscesBuffer::handlePayload(Event* ev)
{
  auto pkt = static_cast<PiscesPacket*>(ev);
  pkt->setArrival(now());
  int dst_vc = pkt->vc();

#if SSTMAC_SANITY_CHECK
  if (dst_vc >= credits_.size()) {
    spkt_abort_printf("pisces_buffer::handlePayload: on %s, port %d, invalid vc %d",
                     toString().c_str(), pkt->edgeOutport(), dst_vc);
  }
#endif

  int& num_credits = credits_[dst_vc];
  pisces_debug(
    "On %s with %d credits, handling payload {%s} for vc:%d",
    toString().c_str(), num_credits,
    pkt->toString().c_str(), dst_vc);

  // it either gets queued or gets sent
  // either way there's a delay accumulating for other messages
  bytes_delayed_ += pkt->numBytes();
  if (num_credits >= pkt->numBytes()) {
    num_credits -= pkt->numBytes();
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

Timestamp
PiscesBuffer::sendPayload(PiscesPacket *pkt)
{
  pkt->setArrival(now());
  int dst_vc = pkt->vc();
  int& num_credits = credits_[dst_vc];
  // it either gets queued or gets sent
  // either way there's a delay accumulating for other messages
  bytes_delayed_ += pkt->numBytes();
  num_credits -= pkt->numBytes();
  return send(arb_, pkt, input_, output_);
}

int
PiscesBuffer::queueLength() const
{
  uint32_t bytes_sending = arb_->bytesSending(now());
  uint32_t total_bytes_pending = bytes_sending + bytes_delayed_;
  int queue_length = total_bytes_pending / packet_size_;
  debug_printf(sprockit::dbg::pisces | sprockit::dbg::pisces_queue,
    "On %s, %u bytes delayed, %u bytes sending, %d total pending, %d packets in queue",
     toString().c_str(),
     bytes_delayed_,
     bytes_sending,
     total_bytes_pending, queue_length);
  return std::max(0, queue_length);
}

}
}
