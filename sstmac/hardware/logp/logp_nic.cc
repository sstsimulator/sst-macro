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

#include <sstmac/hardware/logp/logp_nic.h>
#include <sstmac/hardware/logp/logp_switch.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/common/event_callback.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/hardware/interconnect/interconnect.h>

namespace sstmac {
namespace hw {

logp_nic::logp_nic(sprockit::sim_parameters* params, node* parent) :
  next_out_free_(0),
  nic(params, parent)
{
  ack_handler_ = new_handler(parent, &node::handle);
  sprockit::sim_parameters* inj_params = params->get_namespace("injection");
  double inj_bw = inj_params->get_bandwidth_param("bandwidth");
  inj_bw_inverse_ = 1.0/inj_bw;
  inj_lat_ = inj_params->get_time_param("latency");
}

timestamp
logp_nic::send_latency(sprockit::sim_parameters *params) const
{
  return params->get_time_param("latency");
}

timestamp
logp_nic::credit_latency(sprockit::sim_parameters *params) const
{
  return params->get_time_param("latency");
}

logp_nic::~logp_nic()
{
  if (ack_handler_) delete ack_handler_;
}

void
logp_nic::mtl_handle(event *ev)
{
  timestamp now_ = now();
  message* msg = static_cast<message*>(ev);
  if (msg->byte_length() < negligible_size_){
    recv_message(msg);
  } else {
    timestamp time_to_recv = inj_bw_inverse_*msg->byte_length();
    timestamp recv_start = now_ - time_to_recv;
    if (recv_start > next_in_free_){
      next_in_free_ = now_;
      recv_message(msg);
    } else {
      next_in_free_ += time_to_recv;
      send_self_event_queue(next_in_free_, new_callback(this, &nic::recv_message, msg));
    }
  }
}

void
logp_nic::do_send(network_message* msg)
{
  uint64_t num_bytes = msg->byte_length();
  timestamp now_ = now();
  timestamp start_send = now_ > next_out_free_ ? now_ : next_out_free_;
  nic_debug("logp injection queued at %8.4e, sending at %8.4e for message %s",
            now_.sec(), start_send.sec(), msg->to_string().c_str());

  timestamp time_to_inject = inj_lat_ + timestamp(inj_bw_inverse_ * num_bytes);
  next_out_free_ = start_send + time_to_inject;

  if (msg->needs_ack()){
    network_message* acker = msg->clone_injection_ack();
    auto ack_ev = new_callback(parent_, &node::handle, acker);
    parent_->send_self_event_queue(next_out_free_, ack_ev);
  }

#if SSTMAC_INTEGRATED_SST_CORE
  timestamp extra_delay = start_send - now_;
  logp_switch_->send_extra_delay(extra_delay, msg);
#else
  logp_switch_->send(start_send, msg);
#endif
}

void
logp_nic::connect_output(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_link* link)
{
#if SSTMAC_INTEGRATED_SST_CORE
  logp_switch_ = link;
#endif
}

void
logp_nic::connect_input(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_link* link)
{
  //nothing needed
}

link_handler*
logp_nic::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new_link_handler(this, &nic::mtl_handle);
#else
  return mtl_handler();
#endif
}

}
}
