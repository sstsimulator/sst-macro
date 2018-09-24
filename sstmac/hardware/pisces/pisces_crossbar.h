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

#ifndef PACKETFLOW_CROSSBAR_H
#define PACKETFLOW_CROSSBAR_H

#include <sstmac/hardware/pisces/pisces_sender.h>
#include <sstmac/hardware/pisces/pisces_stats_fwd.h>
#include <sstmac/common/stats/stat_global_int_fwd.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/keyword_registration.h>

#include <memory>

#define identity_port_mapper(n) std::unique_ptr<pisces_NtoM_queue::port_mapper> (new pisces_NtoM_queue::identity_mapper())
#define constant_port_mapper(n) std::unique_ptr<pisces_NtoM_queue::port_mapper> (new pisces_NtoM_queue::constant_mapper(n))
#define offset_port_mapper(n) std::unique_ptr<pisces_NtoM_queue::port_mapper> (new pisces_NtoM_queue::offset_mapper(n))
#define divide_port_mapper(n) std::unique_ptr<pisces_NtoM_queue::port_mapper> (new pisces_NtoM_queue::div_mapper(n))
#define mod_port_mapper(n) std::unique_ptr<pisces_NtoM_queue::port_mapper> (new pisces_NtoM_queue::mod_mapper(n))

namespace sstmac {
namespace hw {

class pisces_NtoM_queue :
  public pisces_sender
{
 public:
  virtual ~pisces_NtoM_queue();

  pisces_NtoM_queue(sprockit::sim_parameters* params,
                         event_scheduler* parent);

  void handle_payload(event* ev) override;

  void handle_credit(event* ev) override;

  event_handler* credit_handler();

  event_handler* payload_handler();

  void set_input(sprockit::sim_parameters* params,
            int my_inport, int src_outport, event_link* link) override;

  void set_output(sprockit::sim_parameters* params,
             int my_outport, int dst_inport, event_link* link) override;

  virtual void start_message(message* msg);

  inline int slot(int port, int vc) const {
    return port * num_vc_ + vc;
  }

  void set_tile_id(std::string id) {
    tile_id_ = id;
  }

  std::string tile_id() const {
    return tile_id_;
  }

  void deadlock_check() override;

  void deadlock_check(event* ev) override;

  class port_mapper
  {
  public:
    port_mapper() {}
    virtual ~port_mapper() {}
    virtual int local_port(const int) const = 0;
  };

  class identity_mapper : public port_mapper
  {
  private:
  public:
    identity_mapper() {}
    ~identity_mapper() {}
    virtual int local_port(const int port) const override {
      return port;
    }
  };

  class constant_mapper : public port_mapper
  {
  private:
    int constant_;
  public:
    constant_mapper(int constant) : constant_(constant) {}
    ~constant_mapper() {}
    virtual int local_port(const int port) const override {
      return constant_;
    }
  };

  class offset_mapper : public port_mapper
  {
  private:
    int offset_;
  public:
    offset_mapper(int offset) : offset_(offset)  {}
    ~offset_mapper() {}
    virtual int local_port(const int port) const override {
      return port - offset_;
    }
  };

  class div_mapper : public port_mapper
  {
  private:
    int div_;
  public:
    div_mapper(int div) : div_(div) {
    }
    ~div_mapper() {}
    virtual int local_port(const int port) const override {
      return port / div_;
    }
  };

  class mod_mapper : public port_mapper
  {
  private:
    int mod_;
  public:
    mod_mapper(int mod) : mod_(mod) {}
    ~mod_mapper() {}
    virtual int local_port(const int port) const override {
      return port % mod_;
    }
  };

  void configure_outports(int num_ports,
                     std::unique_ptr<port_mapper> mapper
                     = std::unique_ptr<port_mapper>(new identity_mapper()),
                     std::unique_ptr<port_mapper> credit_mapper
                     = std::unique_ptr<port_mapper>(new identity_mapper()) ) {
    resize(num_ports);
    outport_mapper_ = std::move(mapper);
    credit_mapper_ = std::move(credit_mapper);
  }

  int local_outport(int port) {
    return outport_mapper_->local_port(port);
  }

  int local_outport_credit(int port) {
    return credit_mapper_->local_port(port);
  }

 protected:
  typedef std::unordered_map<int, pisces_input> input_map;

  typedef std::vector<pisces_output> output_map;
  typedef std::vector<int> credit_map;
  typedef std::vector<payload_queue> queue_map;

  pisces_bandwidth_arbitrator* arb_;

  input_map inputs_;
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
  event_handler* payload_handler_;

  std::map<int, std::set<int> > deadlocked_channels_;

  std::map<int, std::map<int, std::list<pisces_payload*> > > blocked_messages_;

  std::string tile_id_;

 protected:
  void send_payload(pisces_payload* pkt);

  void build_blocked_messages();

 private:
  std::unique_ptr<port_mapper> outport_mapper_;
  std::unique_ptr<port_mapper> credit_mapper_;

  inline int& credit(int port, int vc){
    return credits_[slot(port, vc)];
  }

  void resize(int num_ports);

  inline payload_queue& queue(int port, int vc){
    return queues_[slot(port, vc)];
  }

  std::string input_name(pisces_payload* pkt);

  std::string output_name(pisces_payload* pkt);

  event_link* output_link(pisces_payload* pkt);

};

class pisces_demuxer :
  public pisces_NtoM_queue
{
 public:
  pisces_demuxer(sprockit::sim_parameters* params,
                      event_scheduler* parent);

  std::string pisces_name() const override {
    return "demuxer";
  }

};


class pisces_muxer :
  public pisces_NtoM_queue
{
 public:
  pisces_muxer(sprockit::sim_parameters* params,
                    event_scheduler* parent);

  std::string pisces_name() const override {
    return "muxer";
  }
};

class pisces_crossbar :
  public pisces_NtoM_queue
{
 public:
  pisces_crossbar(sprockit::sim_parameters* params,
                       event_scheduler* parent);

  std::string pisces_name() const override {
    return "crossbar";
  }
};


}
}

#endif // PACKETFLOW_CROSSBAR_H
