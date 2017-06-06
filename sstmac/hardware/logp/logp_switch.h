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

#ifndef SIMPLE_SWITCH_H
#define SIMPLE_SWITCH_H

#include <sstmac/common/event_handler.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/common/messages/sst_message_fwd.h>
#include <sstmac/hardware/nic/nic_fwd.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sprockit/unordered.h>

namespace sstmac {
namespace hw {

/**
 * @brief Implements a switch that does very basic congestion modeling
 *        using the LogGP model.  See "LogGP in Theory and Practice"
 *        by Hoefler and Schneider.
 */
class logp_switch :
  public network_switch
{
 public:
  RegisterComponent("logP | simple | LogP | logp", network_switch, logp_switch,
         "macro", COMPONENT_CATEGORY_NETWORK,
         "A switch that implements a basic delay model with no congestion modeling")

 public:
  typedef enum {
    Node,
    Switch
  } Port;

  logp_switch(sprockit::sim_parameters* params, uint64_t id, event_manager* mgr);

  void handle(event* ev);

  std::string to_string() const override {
    return "LogP switch";
  }

  int queue_length(int port) const override {
    return 0;
  }

  virtual ~logp_switch();

  void connect_output(sprockit::sim_parameters *params,
                      int src_outport, int dst_inport,
                      event_handler* handler) override;

  void connect_input(sprockit::sim_parameters *params,
                     int src_outport, int dst_inport,
                     event_handler* handler) override;

  link_handler* payload_handler(int port) const override;

  link_handler* credit_handler(int port) const override {
    return nullptr;
  }

 private:
  void incoming_message(message* msg, node_id src, node_id dst);
  void outgoing_message(message* msg, node_id src, node_id dst);
  void bcast_local_message(message* msg, node_id src);
  void forward_bcast_message(message* msg, node_id dst);

 private:
  double inj_bw_inverse_;

  timestamp inj_lat_;

  timestamp dbl_inj_lat_;

  double inverse_bw_;

  double inv_min_bw_;

  timestamp hop_latency_;

  interconnect* interconn_;

  std::vector<event_handler*> neighbors_;
  std::vector<event_handler*> nics_;

#if !SSTMAC_INTEGRATED_SST_CORE
  link_handler* mtl_handler_;
#endif

};

}
}


#endif // SIMPLE_SWITCH_H
