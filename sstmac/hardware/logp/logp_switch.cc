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
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/software/launch/launch_event.h>
#include <random>

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

LogPSwitch::LogPSwitch(uint32_t cid, SST::Params& params) :
  ConnectableComponent(cid, params),
  rng_(nullptr), contention_model_(nullptr)
{
  SST::Params topParams;
  top_ = Topology::staticTopology(topParams);

  std::string net_bw = params.find<std::string>("bandwidth");
  byte_delay_ = TimeDelta(SST::UnitAlgebra(net_bw).getValue().inverse().toDouble());

  hop_latency_ = TimeDelta(params.find<SST::UnitAlgebra>("hop_latency").getValue().toDouble());

  out_in_lat_ = TimeDelta(params.find<SST::UnitAlgebra>("out_in_latency").getValue().toDouble());

  if (params.contains("random_seed")){
    random_seed_ = params.find<int>("random_seed");
    rng_ = RNG::MWC::construct();
    random_max_extra_latency_ = TimeDelta(params.find<SST::UnitAlgebra>("random_max_extra_latency").getValue().toDouble());
    random_max_extra_byte_delay_ = TimeDelta(params.find<SST::UnitAlgebra>("random_max_extra_byte_delay").getValue().toDouble());
  }

  SST::Params contention_params = params.find_scoped_params("contention");
  if (contention_params.contains("model")){
    contention_model_ = sprockit::create<ContentionModel>(
      "macro", contention_params.find<std::string>("model"), contention_params);
  }

  nic_links_.resize(top_->numNodes());

  initLinks(params);
}

LogPSwitch::~LogPSwitch()
{
  if (rng_) delete rng_;
  // JJW 4/10/19 these are now owned by the interconnect
  //for (auto* link : nic_links_){
  //  delete link;
  //}
}

void
LogPSwitch::sendEvent(Event *ev)
{
  NicEvent* nev = dynamic_cast<NicEvent*>(ev);
  NetworkMessage* msg = nev->msg();
  delete nev;
  send(now(), msg);
}

void
LogPSwitch::send(Timestamp start, NetworkMessage* msg)
{
  TimeDelta delay;
  if (rng_){
    uint64_t t = start.time.ticks();
    t = ((t*random_seed_) << 5) + random_seed_;
    uint32_t z = (uint32_t)((t & 0xFFFFFFFF00000000LL) >> 32);
    uint32_t w = (uint32_t)(t & 0xFFFFFFFFLL);
    rng_->reseed(z, w);
    double lat_inc = rng_->realvalue();
    delay += lat_inc * random_max_extra_latency_;
    double bw_inc = rng_->realvalue();
    delay += msg->byteLength() * bw_inc * random_max_extra_byte_delay_;
  } else if (contention_model_) {
    double contention = contention_model_->value();
    delay += msg->byteLength() * byte_delay_ * contention;
  }

  NodeId dst = msg->toaddr();
  delay += byte_delay_ * msg->byteLength(); //bw term
  int num_hops = top_->numHopsToNode(msg->fromaddr(), dst);
  delay += num_hops * hop_latency_;
  debug_printf(sprockit::dbg::logp,
               "sending flow %llu over %d hops with extra delay %12.8e and inj lat %12.8e for byte delay %12.8e/bw %12.8e on size %d: %s",
               msg->flowId(), num_hops, delay.sec(), out_in_lat_.sec(),
               byte_delay_.sec(), 1.0/byte_delay_.sec(),
               msg->byteLength(), msg->toString().c_str());

  TimeDelta extra_delay = start - now() + delay;

  nic_links_[dst]->send(extra_delay, new NicEvent(msg));
}

struct SlidingContentionModel : public LogPSwitch::ContentionModel
{
 public:
  SST_ELI_REGISTER_DERIVED(
    LogPSwitch::ContentionModel,
    SlidingContentionModel,
    "macro",
    "sliding",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "")

  SlidingContentionModel(SST::Params& params) : LogPSwitch::ContentionModel(params)
  {
    range_ = params.find<int>("range", 100);
    if (params.contains("cutoffs")){
      params.find_array("cutoffs", cutoffs_);
    } else {
      cutoffs_.resize(2);
      cutoffs_[0] = 60;
      cutoffs_[1] = 90;
    }

    std::random_device rd;  //Will be used to obtain a seed for the random number engine
    std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
    state_ = gen();
  }

  double value() override {
    int num = xorshift64() % range_;
    for (int i=cutoffs_.size() - 1; i >= 0; --i){
      if (num > cutoffs_[i]){
        return i + 1;
      }
    }
    return 0; //no contention
  }

 private:
  uint64_t xorshift64()
  {
    uint64_t x = state_;
    x^= x << 13;
    x^= x >> 7;
    x^= x << 17;
    state_ = x;
    return x;
  }

  int range_;
  std::vector<int> cutoffs_;
  uint64_t state_;

};


}
}

#endif // simple_switch_CC
