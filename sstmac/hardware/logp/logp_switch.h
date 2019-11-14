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

#ifndef SIMPLE_SWITCH_H
#define SIMPLE_SWITCH_H

#include <sstmac/common/event_handler.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/rng.h>
#include <sstmac/sst_core/integrated_component.h>
#include <sstmac/hardware/common/flow_fwd.h>
#include <sstmac/hardware/network/network_message_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/hardware/common/connection.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <unordered_map>

namespace sstmac {
namespace hw {

/**
 * @brief Implements a switch that does very basic congestion modeling
 *        using the LogGP model.  See "LogGP in Theory and Practice"
 *        by Hoefler and Schneider.
 */
class LogPSwitch : public ConnectableComponent
{

 public:
  SST_ELI_REGISTER_COMPONENT(
    LogPSwitch,
    "macro",
    "logp_switch",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "A switch that implements a basic delay model with no congestion modeling",
    COMPONENT_CATEGORY_NETWORK)

  SST_ELI_DOCUMENT_PORTS(SSTMAC_VALID_PORTS)

  struct ContentionModel {
    SST_ELI_DECLARE_BASE(ContentionModel)
    SST_ELI_DECLARE_DEFAULT_INFO()
    SST_ELI_DECLARE_CTOR(SST::Params&)

    virtual double value() = 0;

    ContentionModel(SST::Params&){}
  };

 public:
  LogPSwitch(uint32_t cid, SST::Params& params);

  virtual ~LogPSwitch();

  std::string toString() const override {
    return "LogP switch";
  }

  void connectOutput(int src_outport, int  /*dst_inport*/, EventLink::ptr&& payload_link) override {
    nic_links_[src_outport] = std::move(payload_link);
  }

  void connectInput(int, int, EventLink::ptr&&) override {}

  LinkHandler* payloadHandler(int  /*port*/) override {
    return newLinkHandler(this, &LogPSwitch::sendEvent);
  }

  LinkHandler* creditHandler(int  /*port*/) override {
    return newLinkHandler(this, &LogPSwitch::dropEvent);
  }

  void connectOutput(NodeId nid, EventLink::ptr&& link) {
    nic_links_[nid] = std::move(link);
  }

  void sendEvent(Event* ev);

  void dropEvent(Event*){}

  void send(NetworkMessage* msg){
    send(now(), msg);
  }

  void send(Timestamp start, NetworkMessage* msg);

  TimeDelta out_in_latency() const {
    return out_in_lat_;
  }

 private:
  TimeDelta inj_lat_;
  TimeDelta out_in_lat_;

  TimeDelta byte_delay_;

  TimeDelta hop_latency_;

  Topology* top_;

  std::vector<EventLink::ptr> nic_links_;

  RNG::MWC* rng_;

  ContentionModel* contention_model_;

  TimeDelta random_max_extra_latency_;
  TimeDelta random_max_extra_byte_delay_;
  uint32_t random_seed_;

};



}
}


#endif // SIMPLE_SWITCH_H
