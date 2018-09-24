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

#ifndef sculpin_nic_h
#define sculpin_nic_h

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/sculpin/sculpin_switch.h>
#include <sstmac/hardware/pisces/pisces_packetizer.h>

namespace sstmac {
namespace hw {

/**
 @class sculpin_nic
 Network interface compatible with sculpin network model
 */
class sculpin_nic :
  public nic
{
  FactoryRegister("sculpin", nic, sculpin_nic,
              "implements a nic that models messages as a packet flow")
 public:
  sculpin_nic(sprockit::sim_parameters* params, node* parent);

  std::string to_string() const override {
    return sprockit::printf("sculpin nic(%d)", int(addr()));
  }

  void init(unsigned int phase) override;

  void setup() override;

  virtual ~sculpin_nic() throw ();

  void handle_payload(event* ev);

  void handle_credit(event* ev);

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

  void deadlock_check() override;

  void deadlock_check(event* ev) override;

 private:
  void do_send(network_message* payload) override;

  void cq_handle(sculpin_packet* pkt);

  void eject(sculpin_packet* pkt);

 private:
  timestamp inj_next_free_;
  event_link* inj_link_;

  double inj_inv_bw_;

  uint32_t packet_size_;

  timestamp ej_next_free_;
  recv_cq cq_;

#if !SSTMAC_INTEGRATED_SST_CORE
  link_handler* payload_handler_;
  link_handler* ack_handler_;
#endif
};

}
} // end of namespace sstmac


#endif // pisces_nic_H
