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

#ifndef PACKETFLOW_CREDITOR_H
#define PACKETFLOW_CREDITOR_H

#include <sprockit/util.h>
#include <sstmac/common/stats/stat_spyplot_fwd.h>
#include <sstmac/hardware/pisces/pisces.h>
#include <sstmac/hardware/pisces/pisces_arbitrator.h>
#include <sstmac/hardware/pisces/pisces_stats.h>
#include <sstmac/common/event_scheduler.h>

#define pisces_debug(...) \
  debug_printf(sprockit::dbg::pisces, __VA_ARGS__)

namespace sstmac {
namespace hw {

struct payload_queue {

  std::list<pisces_payload*> queue;

  typedef std::list<pisces_payload*>::iterator iterator;

  pisces_payload* pop(int num_credits);

  pisces_payload* front();

  size_t size() const {
    return queue.size();
  }

  void push_back(pisces_payload* payload);

};

struct pisces_input {
  int src_outport;
  event_handler* handler;
  pisces_input() :
    src_outport(-1),
    handler(0)
  {
  }
};

struct pisces_output {
  int dst_inport;
  event_handler* handler;
  pisces_output() :
    dst_inport(-1),
    handler(0)
  {
  }
};

class pisces_sender :
  public event_subcomponent
{
  DeclareFactory(pisces_sender, event_component*)
 public:
  virtual ~pisces_sender() {}

  virtual void set_input(sprockit::sim_parameters* params,
     int my_inport, int dst_outport,
     event_handler* input) = 0;

  virtual void set_output(sprockit::sim_parameters* params,
    int my_outport, int dst_inport,
    event_handler* output) = 0;

  virtual void handle_credit(event* ev) = 0;

  virtual void handle_payload(event* ev) = 0;

  static void configure_credit_port_latency(sprockit::sim_parameters* params);

  static void configure_payload_port_latency(sprockit::sim_parameters* params);

  void set_stat_collector(packet_stats_callback* c){
    stat_collector_ = c;
  }

  virtual std::string pisces_name() const = 0;

  std::string to_string() const override;

  void set_update_vc(bool flag){
    update_vc_ = flag;
  }

 protected:
  pisces_sender(sprockit::sim_parameters* params,
                     event_scheduler* parent);

  virtual void send_credit(const pisces_input& src,
    pisces_payload* payload,
    timestamp packet_tail_leaves);

  void send(pisces_bandwidth_arbitrator* arb,
       pisces_payload* pkt,
       const pisces_input& src,
       const pisces_output& dest);

 protected:
  packet_stats_callback* stat_collector_;

  timestamp send_lat_;

  timestamp credit_lat_;

  bool update_vc_;

};

}
}

#endif // PACKETFLOW_CREDITOR_H