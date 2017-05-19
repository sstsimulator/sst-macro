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

#ifndef pisces_BUFFER_H
#define pisces_BUFFER_H

#include <sstmac/hardware/pisces/pisces_crossbar.h>
#include <sstmac/hardware/pisces/pisces_sender.h>

namespace sstmac {
namespace hw {


class pisces_buffer :
  public pisces_sender
{

 public:
  virtual ~pisces_buffer();

  virtual void
  set_output(sprockit::sim_parameters* params,
    int this_outport, int dst_inport,
    event_handler* output) override;

  virtual void
  set_input(
    sprockit::sim_parameters* params,
    int this_inport, int src_outport,
    event_handler* input) override;

  virtual int
  queue_length() const {
    return 0;
  }

  device_id
  output_location() const {
    return output_.handler->event_location();
  }

  device_id
  input_location() const {
    return input_.handler->event_location();
  }

 protected:
  pisces_buffer(sprockit::sim_parameters* params, event_scheduler* parent);

 protected:
  pisces_input input_;
  pisces_output output_;
  long bytes_delayed_;

  static const int my_outport = 0;
  static const int my_inport = 0;
};

class pisces_network_buffer :
  public pisces_buffer
{
 public:
  pisces_network_buffer(sprockit::sim_parameters* params,
                             event_scheduler* parent);

  virtual ~pisces_network_buffer();

  int
  queue_length() const override;

  void
  handle_credit(event* ev) override;

  void
  handle_payload(event* ev) override;

  std::string
  pisces_name() const override {
    return "network buffer";
  }

  event_handler*
  payload_handler();

  void deadlock_check() override;

  void deadlock_check(event* ev) override;

 protected:
  int num_vc_;
  std::vector<payload_queue> queues_;
  std::vector<int> credits_;

 private:
  void build_blocked_messages();

 private:
  pisces_bandwidth_arbitrator* arb_;
  std::set<int> deadlocked_channels_;
  std::map<int, std::list<pisces_payload*> > blocked_messages_;
  int packet_size_;
  event_handler* payload_handler_;
};

class pisces_eject_buffer :
  public pisces_buffer
{
 public:
  pisces_eject_buffer(sprockit::sim_parameters* params,
                           event_scheduler* parent) :
    pisces_buffer(params, parent)
  {
  }

  void
  handle_credit(event* ev) override;

  void
  handle_payload(event* ev) override;

  void
  return_credit(packet* msg);

  std::string
  pisces_name() const override {
    return "eject buffer";
  }

};

class pisces_injection_buffer :
  public pisces_buffer
{
 public:
  pisces_injection_buffer(sprockit::sim_parameters* params,
                               event_scheduler* parent);

  ~pisces_injection_buffer();

  int
  queue_length() const override;

  bool
  space_to_send(int bytes) const {
    return credits_ >= bytes;
  }

  void
  handle_credit(event* ev) override;

  void
  handle_payload(event* ev) override;

  std::string
  pisces_name() const override {
    return "inject buffer";
  }

 protected:
  int packet_size_;
  pisces_bandwidth_arbitrator* arb_;
  long credits_;

};

}
}


#endif // BUFFER_H