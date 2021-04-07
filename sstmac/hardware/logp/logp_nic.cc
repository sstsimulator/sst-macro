/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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

#include <sstmac/hardware/logp/logp_nic.h>
#include <sstmac/hardware/logp/logp_switch.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/event_callback.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/hardware/interconnect/interconnect.h>

namespace sstmac {
namespace hw {

LogPNIC::LogPNIC(uint32_t id, SST::Params& params, Node* node) :
  NIC(id, params, node),
  next_out_free_()
{
  SST::Params inj_params = params.find_scoped_params("injection");
  inj_byte_delay_ = TimeDelta(inj_params.find<SST::UnitAlgebra>("bandwidth").getValue().inverse().toDouble());
  inj_lat_ = TimeDelta(inj_params.find<SST::UnitAlgebra>("latency").getValue().toDouble());

  configureLogPLinks();
}

LogPNIC::~LogPNIC()
{
}

void
LogPNIC::mtlHandle(Event *ev)
{
  Timestamp now_ = now();
  NicEvent* nev = static_cast<NicEvent*>(ev);
  NetworkMessage* msg = nev->msg();
  delete nev;
  if (msg->byteLength() < negligibleSize_){
    recvMessage(msg);
  } else {
    TimeDelta time_to_recv = inj_byte_delay_*msg->byteLength();
    Timestamp recv_start = now_ - time_to_recv;
    if (recv_start > next_in_free_){
      next_in_free_ = now_;
      recvMessage(msg);
    } else {
      next_in_free_ += time_to_recv;
      sendExecutionEvent(next_in_free_, newCallback(this, &NIC::recvMessage, msg));
    }
  }
}

void
LogPNIC::doSend(NetworkMessage* msg)
{
  uint64_t num_bytes = msg->byteLength();
  Timestamp now_ = now();
  Timestamp start_send = now_ > next_out_free_ ? now_ : next_out_free_;
  nic_debug("logp injection queued at %8.4e, sending at %8.4e for %s",
            now_.sec(), start_send.sec(), msg->toString().c_str());

  TimeDelta time_to_inject = inj_lat_ + inj_byte_delay_ * num_bytes;
  next_out_free_ = start_send + time_to_inject;

  if (msg->needsAck()){
    NetworkMessage* acker = msg->cloneInjectionAck();
    auto ack_ev = newCallback(parent_, &Node::handle, acker);
    parent_->sendExecutionEvent(next_out_free_, ack_ev);
  }

  TimeDelta extra_delay = start_send - now_;
  logp_link_->send(extra_delay, new NicEvent(msg));
}

void
LogPNIC::connectOutput(int  /*src_outport*/, int  /*dst_inport*/, EventLink::ptr&& link)
{
  logp_link_ = std::move(link);
}

void
LogPNIC::connectInput(int /*src_outport*/, int /*dst_inport*/, 
                      EventLink::ptr&& /*link*/)
{
  //nothing needed
}

LinkHandler*
LogPNIC::payloadHandler(int  /*port*/)
{
  return newLinkHandler(this, &NIC::mtlHandle);
}

}
}
