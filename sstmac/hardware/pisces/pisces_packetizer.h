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

#ifndef pisces_packetizer_H
#define pisces_packetizer_H

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/common/packetizer.h>
#include <sstmac/common/stats/stat_histogram.h>
#include <sstmac/hardware/pisces/packet_allocator_fwd.h>
#include <sstmac/hardware/network/network_message.h>

namespace sstmac {
namespace hw {

/**
 @class pisces_packetizer
 */
class pisces_packetizer :
  public packetizer
{
 public:
  pisces_packetizer(sprockit::sim_parameters* params,
                    event_scheduler* parent);

  virtual ~pisces_packetizer();

  void init(unsigned int phase) override;

  void setup() override;

  bool spaceToSend(int vn, int num_bits) override;

  void inject(int vn, long bytes, long byte_offset, message *payload) override;

  /**
   Set up the injection/ejection links to the switch the NIC is connected to
   @param sw The switch that injects/ejects
   */
  void set_output(sprockit::sim_parameters* params,
             int port, event_handler* output);

  void set_input(sprockit::sim_parameters* params,
            int port, event_handler* input);

  void recv_credit(event* credit);

  /**
   * @brief recv_packet Receive new packet arriving from network.
   *  Assemble packet into corresponding message (flow)
   * @param ev
   */
  virtual void recv_packet(event* ev) = 0;

  link_handler* new_payload_handler() const override;

  link_handler* new_credit_handler() const override;

  void deadlock_check() override;

 protected:
  void recv_packet_common(pisces_payload* pkt);

 private:
  void init(sprockit::sim_parameters* params, event_scheduler* parent);

 protected:
  pisces_injection_buffer* inj_buffer_;
  pisces_eject_buffer* ej_buffer_;

  event_handler* payload_handler_;

  recv_cq completion_queue_;

  node_id my_addr_;

  packet_stats_callback* inj_stats_;
  packet_stats_callback* ej_stats_;
  packet_allocator* pkt_allocator_;

};

/**
 * @brief The pisces_cut_through_packetizer class
 * See #pisces_cut_through_bandwidth_arbitrator.
 */
class pisces_cut_through_packetizer : public pisces_packetizer
{
  FactoryRegister("cut_through | null", packetizer, pisces_cut_through_packetizer)
 public:
  pisces_cut_through_packetizer(sprockit::sim_parameters* params,
                                event_scheduler* parent) :
    pisces_packetizer(params, parent)
  {
  }

  /**
   * @brief recv_packet Invoked when head flit arrives for packet.
   *  Cut-through delays must be added before packet can be processed.
   * @param pkt
   */
  void recv_packet(event* pkt) override;

  std::string to_string() const override {
    return "cut through packetizer";
  }

};

/**
 * @brief The pisces_simple_packetizer class
 */
class pisces_simple_packetizer : public pisces_packetizer
{
  FactoryRegister("simple", packetizer, pisces_simple_packetizer)
 public:
  pisces_simple_packetizer(sprockit::sim_parameters* params,
                           event_scheduler* parent) :
    pisces_packetizer(params, parent)
  {
  }

  std::string to_string() const override {
    return "simple packetizer";
  }

  /**
   * @brief recv_packet Invoked when tail flit arrives for packet.
   *  No extra delays must be processed for packet
   * @param pkt
   */
  void recv_packet(event* pkt) override;

};

}
} // end of namespace sstmac

#endif // pisces_packetizer_H