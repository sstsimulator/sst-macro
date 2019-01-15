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
  DeclareFactory(LogPSwitch,uint32_t)

 public:
  RegisterComponent("logP | simple | LogP | logp", LogPSwitch, LogPSwitch,
         "macro", COMPONENT_CATEGORY_NETWORK,
         "A switch that implements a basic delay model with no congestion modeling")

  struct ContentionModel {
    DeclareFactory(ContentionModel)
    virtual double value() = 0;
  };

 public:
  LogPSwitch(sprockit::sim_parameters::ptr& params, uint32_t cid);

  virtual ~LogPSwitch();

  std::string toString() const override {
    return "LogP switch";
  }

  void connectOutput(sprockit::sim_parameters::ptr& params,
                      int src_outport, int dst_inport,
                      EventLink *payload_link) override {
    nic_links_[src_outport] = payload_link;
  }

  void connectInput(sprockit::sim_parameters::ptr& params,
                     int src_outport, int dst_inport,
                     EventLink *credit_link) override {
    //do nothing
  }

  LinkHandler* payloadHandler(int port) override {
    return newLinkHandler(this, &LogPSwitch::sendEvent);
  }

  LinkHandler* creditHandler(int port) override {
    return newLinkHandler(this, &LogPSwitch::dropEvent);
  }

  void connectOutput(NodeId nid, EventLink* link){
    nic_links_[nid] = link;
  }

  void sendEvent(Event* ev);

  void dropEvent(Event* ev){}

  void send(NetworkMessage* msg){
    send(now(), msg);
  }

  void send(Timestamp start, NetworkMessage* msg);

  Timestamp sendLatency(sprockit::sim_parameters::ptr& params) const override {
    return out_in_lat_;
  }

  Timestamp creditLatency(sprockit::sim_parameters::ptr& params) const override {
    return out_in_lat_;
  }

 private:
  double inj_bw_inverse_;

  Timestamp inj_lat_;

  Timestamp out_in_lat_;

  double inverse_bw_;

  double inv_min_bw_;

  Timestamp hop_latency_;

  Topology* top_;

  std::vector<EventLink*> nic_links_;

  RNG::MWC* rng_;

  ContentionModel* contention_model_;

  Timestamp random_max_extra_latency_;
  double random_max_extra_byte_delay_;
  uint32_t random_seed_;

};



}
}


#endif // SIMPLE_SWITCH_H
