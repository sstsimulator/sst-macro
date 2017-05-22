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

#ifndef PACKETFLOW_CROSSBAR_H
#define PACKETFLOW_CROSSBAR_H

#include <sstmac/hardware/pisces/pisces_sender.h>
#include <sstmac/hardware/pisces/pisces_stats_fwd.h>
#include <sstmac/common/stats/stat_global_int_fwd.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/keyword_registration.h>

namespace sstmac {
namespace hw {

class pisces_NtoM_queue :
  public pisces_sender
{
 public:
  virtual ~pisces_NtoM_queue();

  pisces_NtoM_queue(sprockit::sim_parameters* params,
                         event_scheduler* parent);

  int
  thread_id() const {
    return event_subcomponent::thread_id();
  }

  void
  handle_payload(event* ev) override;

  void
  handle_credit(event* ev) override;

  event_handler*
  credit_handler();

  void
  set_input(sprockit::sim_parameters* params,
            int my_inport, int src_outport, event_handler* input) override;

  void
  set_output(sprockit::sim_parameters* params,
             int my_outport, int dst_inport, event_handler* output) override;

  virtual void
  start_message(message* msg);

  void
  configure_mod_ports(int mod);

  void
  configure_div_ports(int div, int max_port);

  void
  configure_offset_ports(int offset, int max_port);

  void
  configure_basic_ports(int num_ports);

  inline int
  local_port(int port) const {
    if (port_mod_){
      return port % port_mod_;
    } else {
      return port / port_div_ - port_offset_;
    }
  }

  inline int
  local_slot(int port, int vc) const {
    return local_port(port) * num_vc_ + vc;
  }

  void
  deadlock_check() override;

  void
  deadlock_check(event* ev) override;

 protected:
  typedef spkt_unordered_map<int, pisces_input> input_map;

  typedef std::vector<pisces_output> output_map;
  typedef std::vector<int> credit_map;
  typedef std::vector<payload_queue> queue_map;

  pisces_bandwidth_arbitrator* arb_;

  input_map inputs_;
  //indexed by slot number = (port,vc)
  output_map outputs_;
  //indexed by slot number = (port,vc)
  credit_map credits_;
  //indexed by slot number = (port,vc)
  queue_map queues_;

  int num_vc_;
  int port_offset_;
  int port_div_;
  int port_mod_;

  event_handler* credit_handler_;

  std::map<int, std::set<int> > deadlocked_channels_;

  std::map<int, std::map<int, std::list<pisces_payload*> > > blocked_messages_;

 protected:
  void
  send_payload(pisces_payload* pkt);

  void
  build_blocked_messages();

 private:
  inline int& credit(int port, int vc){
    return credits_[local_slot(port, vc)];
  }

  void resize(int num_ports);

  inline payload_queue& queue(int port, int vc){
    return queues_[local_slot(port, vc)];
  }

  std::string
  input_name(pisces_payload* pkt);

  std::string
  output_name(pisces_payload* pkt);

  event_handler*
  output_handler(pisces_payload* pkt);

};

class pisces_demuxer :
  public pisces_NtoM_queue
{
 public:
  pisces_demuxer(sprockit::sim_parameters* params,
                      event_scheduler* parent);

  std::string
  pisces_name() const override {
    return "demuxer";
  }


};


class pisces_muxer :
  public pisces_NtoM_queue
{
 public:
  pisces_muxer(sprockit::sim_parameters* params,
                    event_scheduler* parent);

  std::string
  pisces_name() const override {
    return "muxer";
  }

};

class pisces_crossbar :
  public pisces_NtoM_queue
{
 public:
  pisces_crossbar(sprockit::sim_parameters* params,
                       event_scheduler* parent);

  std::string
  pisces_name() const override {
    return "crossbar";
  }

};


}
}

#endif // PACKETFLOW_CROSSBAR_H