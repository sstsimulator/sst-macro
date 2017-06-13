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

#ifndef PACKETFLOW_SWITCH_H
#define PACKETFLOW_SWITCH_H

#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/pisces/pisces_buffer.h>
#include <sstmac/hardware/pisces/pisces_crossbar.h>
#include <sstmac/hardware/pisces/pisces_arbitrator.h>
#include <sstmac/hardware/pisces/pisces_stats_fwd.h>

namespace sstmac {
namespace hw {

class pisces_abstract_switch :
  public network_switch
{
 public:
  packet_stats_callback* xbar_stats() const {
    return xbar_stats_;
  }

  packet_stats_callback* buf_stats() const {
    return buf_stats_;
  }

 protected:
  pisces_abstract_switch(
    sprockit::sim_parameters* params,
    uint64_t id,
    event_manager* mgr);

  virtual ~pisces_abstract_switch();

  packet_stats_callback* xbar_stats_;
  packet_stats_callback* buf_stats_;
  router* router_;
};

/**
 @class pisces_switch
 A switch in the network that arbitrates/routes packet_trains
 to the next link in the network
 */
class pisces_switch :
  public pisces_abstract_switch
{
  RegisterComponent("pisces", network_switch, pisces_switch,
         "macro", COMPONENT_CATEGORY_NETWORK,
         "A network switch implementing the packet flow congestion model")
 public:
  pisces_switch(sprockit::sim_parameters* params, uint64_t id, event_manager* mgr);

  virtual ~pisces_switch();

  int queue_length(int port) const override;

  virtual void connect_output(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_handler* mod) override;

  virtual void connect_input(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_handler* mod) override;

  link_handler* credit_handler(int port) const override;

  link_handler* payload_handler(int port) const override;

  void handle_credit(event* ev);

  void handle_payload(event* ev);

  void deadlock_check() override;

  void deadlock_check(event* ev) override;

  virtual void compatibility_check() const override;

  /**
   Set the link to use when ejecting packets at their endpoint.  A pisces_switch
   can have any number of ejectors, corresponding to the number of nodes
   per switch.
   @param addr The compute node address of the endpoint to eject to
   @param link The link to the compute node for ejection
   */
  void add_ejector(node_id addr, event_handler* link);

  virtual std::string to_string() const override;

 private:
  std::vector<pisces_sender*> out_buffers_;

  pisces_crossbar* xbar_;

#if !SSTMAC_INTEGRATED_SST_CORE
  link_handler* ack_handler_;
  link_handler* payload_handler_;
  event_handler* pkt_handler_; //handles either type
#endif

 private:
  void resize_buffers();

  pisces_sender* output_buffer(sprockit::sim_parameters* params, int port);

};

}
}

#endif // PACKETFLOW_SWITCH_H