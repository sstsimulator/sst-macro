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
#include <sstmac/common/messages/sst_message_fwd.h>
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
class logp_switch : public connectable_component
{
  DeclareFactory(logp_switch,uint32_t,event_manager*)

 public:
  RegisterComponent("logP | simple | LogP | logp", logp_switch, logp_switch,
         "macro", COMPONENT_CATEGORY_NETWORK,
         "A switch that implements a basic delay model with no congestion modeling")

 public:
  logp_switch(sprockit::sim_parameters* params, uint32_t cid, event_manager* mgr);

  virtual ~logp_switch();

  std::string to_string() const override {
    return "LogP switch";
  }

  void connect_output(sprockit::sim_parameters *params,
                      int src_outport, int dst_inport,
                      event_link *payload_link) override {
    nic_links_[src_outport] = payload_link;
  }

  void connect_input(sprockit::sim_parameters *params,
                     int src_outport, int dst_inport,
                     event_link *credit_link) override {
    //do nothing
  }

  link_handler* payload_handler(int port) const override {
    return new_link_handler(this, &logp_switch::send_event);
  }

  link_handler* credit_handler(int port) const override {
    return new_link_handler(this, &logp_switch::drop_event);
  }

  void connect_output(node_id nid, event_link* link){
    nic_links_[nid] = link;
  }

  void send_event(event* ev);

  void drop_event(event* ev){}

  void send(message *msg){
    send(now(), msg);
  }

  void send(timestamp start, message* msg);

  timestamp send_latency(sprockit::sim_parameters* params) const override {
    return out_in_lat_;
  }

  timestamp credit_latency(sprockit::sim_parameters* params) const override {
    return out_in_lat_;
  }

 private:
  double inj_bw_inverse_;

  timestamp inj_lat_;

  timestamp out_in_lat_;

  double inverse_bw_;

  double inv_min_bw_;

  timestamp hop_latency_;

  topology* top_;

  std::vector<event_link*> nic_links_;

  RNG::MWC* rng_;

  timestamp random_max_extra_latency_;
  double random_max_extra_byte_delay_;
  uint32_t random_seed_;

};



}
}


#endif // SIMPLE_SWITCH_H
