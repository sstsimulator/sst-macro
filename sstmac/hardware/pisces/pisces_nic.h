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

#ifndef sstmac_hardware_nic_pisces_nic_H
#define sstmac_hardware_nic_pisces_nic_H

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/pisces/pisces_packetizer.h>
#include <sstmac/common/stats/stat_histogram.h>

namespace sstmac {
namespace hw {

/**
 @class pisces_nic
 Network interface compatible with sending packet trains
 */
class pisces_nic :
  public nic,
  public packetizer_callback
{
  FactoryRegister("pisces | pisces", nic, pisces_nic,
              "implements a nic that models messages as a packet flow")
 public:
  pisces_nic(sprockit::sim_parameters* params, node* parent);

  std::string to_string() const override {
    return sprockit::printf("packet flow nic(%d)", int(addr()));
  }

  void init(unsigned int phase) override;

  void setup() override;

  virtual ~pisces_nic() throw ();

  void notify(int vn, message* msg) override {
    recv_message(msg);
  }

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

  void deadlock_check() override;

 protected:
  virtual void do_send(network_message* payload) override;

 protected:
  packetizer* packetizer_;
#if !SSTMAC_INTEGRATED_SST_CORE
  link_handler* payload_handler_;
  link_handler* ack_handler_;
#endif
};

class pisces_netlink :
  public netlink
{
  FactoryRegister("pisces | pisces", netlink, pisces_netlink,
              "implements a netlink that models messages as a packet flow")
 public:
  pisces_netlink(sprockit::sim_parameters* params, node* parent);

  virtual ~pisces_netlink();

  std::string
  to_string() const override {
    return "packet flow netlink";
  }

  void handle_credit(event* ev);

  void handle_payload(event* ev);

  void deadlock_check() override;

  link_handler* payload_handler(int port) const override;

  link_handler* credit_handler(int port) const override;

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

 private:
  static const int really_big_buffer;
  pisces_crossbar* inj_block_;
  pisces_crossbar* ej_block_;
  int tile_rotater_;
  bool inited_;
#if !SSTMAC_INTEGRATED_SST_CORE
  link_handler* payload_handler_;
  link_handler* ack_handler_;
#endif
};

}
} // end of namespace sstmac


#endif // pisces_nic_H