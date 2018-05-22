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

#ifndef sculpin_switch_h
#define sculpin_switch_h

#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/sculpin/sculpin.h>

namespace sstmac {
namespace hw {

/**
 @class sculpin_switch
 A switch in the network that arbitrates/routes
 to the next link in the network
 */
class sculpin_switch :
  public network_switch
{
  RegisterComponent("sculpin", network_switch, sculpin_switch,
         "macro", COMPONENT_CATEGORY_NETWORK,
         "A network switch implementing the sculpin model")
 public:
  sculpin_switch(sprockit::sim_parameters* params, uint32_t id, event_manager* mgr);

  virtual ~sculpin_switch();

  int queue_length(int port) const override;

  void connect_output(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_link* link) override;

  void connect_input(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_link* link) override;

  link_handler* credit_handler(int port) const override;

  link_handler* payload_handler(int port) const override;

  timestamp send_latency(sprockit::sim_parameters *params) const override;

  timestamp credit_latency(sprockit::sim_parameters *params) const override;

  void handle_credit(event* ev);

  void handle_payload(event* ev);

  void deadlock_check() override;

  void deadlock_check(event* ev) override;

  /**
   * @brief compatibility_check
   * Perform a self-consistency check (before sim starts) on all components.
   * This usually involves checking dynamic types that cannot be verified at compile-time
   * and are difficult to detect directly from the parameters (hence would otherwise fail in ctor).
   */
  void compatibility_check() const override;

  std::string to_string() const override;

 private:
  struct priority_compare {
    bool operator()(sculpin_packet* l, sculpin_packet* r) const {
      if (l->priority() == r->priority()){
        if (l->arrival() == r->arrival()){
          return l->seqnum() < r->seqnum();
        } else {
          return l->arrival() < r->arrival();
        }
      } else {
        //zero is highest priority
        return l->priority() < r->priority();
      }
    }
  };

  struct port {
    int id;
    timestamp next_free;
    double inv_bw;
    uint32_t seqnum;
    std::set<sculpin_packet*, priority_compare> priority_queue;
    event_link* link;
  };

  std::vector<port> ports_;

  router* router_;

#if !SSTMAC_INTEGRATED_SST_CORE
  link_handler* payload_handler_;
  link_handler* credit_handler_;
#endif

  bool congestion_;

 private:
  void send(port& p, sculpin_packet* pkt, timestamp now);

  void try_to_send_packet(sculpin_packet* pkt);

  void pull_next(int portnum);

};

}
}

#endif // PACKETFLOW_SWITCH_H
