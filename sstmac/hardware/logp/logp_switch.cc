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

#ifndef simple_switch_CC
#define simple_switch_CC

/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009-2018 NTESS.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-NA0003525 with NTESS,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/logp/logp_switch.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/software/launch/launch_event.h>

MakeDebugSlot(logp)

RegisterKeywords(
 { "bandwidth", "" },
 { "hop_latency", "" },
 { "out_in_latency", "" },
 { "random_seed", "a seed for creating randomized message arrivals"},
 { "random_max_extra_latency", "the maximum extra latency allowed in random scenarios"},
 { "random_max_extra_byte_delay", "the maximum extra delay per byte in random scenarios"},
);

namespace sstmac {
namespace hw {

logp_switch::logp_switch(sprockit::sim_parameters *params, uint32_t cid, event_manager* mgr) :
  connectable_component(params, cid, mgr), rng_(nullptr)
{
  top_ = topology::static_topology(nullptr);

  double net_bw = params->get_bandwidth_param("bandwidth");
  inverse_bw_ = 1.0/net_bw;

  double inj_bw = params->get_optional_namespace("ejection")->get_optional_bandwidth_param("bandwidth", net_bw);
  inj_bw_inverse_ = 1.0/inj_bw;

  inv_min_bw_ = std::max(inverse_bw_, inj_bw_inverse_);

  hop_latency_ = params->get_time_param("hop_latency");

  out_in_lat_ = params->get_time_param("out_in_latency");

  if (params->has_param("random_seed")){
    random_seed_ = params->get_int_param("random_seed");
    rng_ = RNG::MWC::construct();
    random_max_extra_latency_ = params->get_time_param("random_max_extra_latency");
    random_max_extra_byte_delay_ = params->get_time_param("random_max_extra_byte_delay");
  }


  nic_links_.resize(top_->num_nodes());

  init_links(params);
}

logp_switch::~logp_switch()
{
  if (rng_) delete rng_;
}

void
logp_switch::send_event(event *ev)
{
  send(now(), dynamic_cast<message*>(ev));
}

void
logp_switch::send(timestamp start, message* msg)
{
  timestamp delay;
  if (rng_){
    uint64_t t = start.ticks();
    t = ((t*random_seed_) << 5) + random_seed_;
    uint32_t z = (uint32_t)((t & 0xFFFFFFFF00000000LL) >> 32);
    uint32_t w = (uint32_t)(t & 0xFFFFFFFFLL);
    rng_->reseed(z, w);
    double lat_inc = rng_->realvalue();
    delay += lat_inc * random_max_extra_latency_;
    double bw_inc = rng_->realvalue();
    delay += msg->byte_length() * bw_inc * random_max_extra_byte_delay_;
  }

  node_id dst = msg->toaddr();
  delay += inv_min_bw_ * msg->byte_length(); //bw term
  int num_hops = top_->num_hops_to_node(msg->fromaddr(), dst);
  delay += num_hops * hop_latency_;
  debug_printf(sprockit::dbg::logp,
               "sending message over %d hops with extra delay %12.8e and inj lat %12.8e: %s",
               num_hops, delay.sec(), out_in_lat_.sec(), msg->to_string().c_str());

  timestamp extra_delay = start - now() + delay;

  event_link* lnk = nic_links_[dst];
  lnk->multi_send_extra_delay(extra_delay, msg, this);
}




}
}

#endif // simple_switch_CC
