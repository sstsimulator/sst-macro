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

#ifndef pisces_BRANCHED_SWITCH_H
#define pisces_BRANCHED_SWITCH_H

#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/pisces/pisces_buffer.h>
#include <sstmac/hardware/pisces/pisces_crossbar.h>
#include <sstmac/hardware/pisces/pisces_arbitrator.h>
#include <sstmac/hardware/pisces/pisces_stats_fwd.h>

namespace sstmac {
namespace hw {

/**
 @class pisces_branched_switch
 A branched/hierarchical switch in the network that arbitrates/routes
 packets to the next link in the network
 */
class pisces_branched_switch :
  public pisces_abstract_switch
{
  RegisterComponent("pisces_branched", network_switch, pisces_branched_switch,
         "macro", COMPONENT_CATEGORY_NETWORK,
         "A branched/hierarchical network switch implementing the packet flow congestion model")
 public:
  pisces_branched_switch(sprockit::sim_parameters* params, uint64_t id, event_manager* mgr);

  int queue_length(int port) const override;

  virtual void connect_output(sprockit::sim_parameters* params,
                 int src_outport, int dst_inport,
                 event_link* link) override;

  virtual void connect_input(sprockit::sim_parameters* params,
                int src_outport, int dst_inport,
                event_link* link) override;

  link_handler* credit_handler(int port) const override;

  link_handler* payload_handler(int port) const override;

  timestamp send_latency(sprockit::sim_parameters *params) const override;

  timestamp credit_latency(sprockit::sim_parameters *params) const override;

  void handle_credit(event* ev);

  void handle_payload(event* ev);

  virtual std::string to_string() const override;

  virtual ~pisces_branched_switch();

  void deadlock_check() override;

  void deadlock_check(event* ev) override;

 protected:
  int n_local_xbars_;
  int n_local_ports_;

  pisces_crossbar* xbar_;
  std::vector<pisces_muxer*> input_muxers_;
  std::vector<pisces_demuxer*> output_demuxers_;

#if !SSTMAC_INTEGRATED_SST_CORE
  link_handler* ack_handler_;
  link_handler* payload_handler_;
#endif

 private:
  void resize_buffers();

  void init_components(sprockit::sim_parameters* params);
};

}
}

#endif // pisces_BRANCHED_SWITCH_H
