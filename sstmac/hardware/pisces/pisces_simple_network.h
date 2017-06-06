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

#ifndef PISCES_SIMPLE_NETWORK_H
#define PISCES_SIMPLE_NETWORK_H

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/common/packetizer.h>
#include <sstmac/common/stats/stat_histogram.h>
#include <sstmac/hardware/pisces/packet_allocator_fwd.h>
#include <sstmac/hardware/network/network_message.h>
#include <sst/core/interfaces/simpleNetwork.h>

namespace sstmac {
namespace hw {

/**
 * @brief The simple_network_packet class  BTL packet that carries requests
 * through the detailed modeling PISCSES network.  The serializable* payload
 * should always be an SST::Request object
 */
class simple_network_packet : public pisces_routable_packet
{
  NotSerializable(simple_network_packet)

 public:
  simple_network_packet(
    serializable* msg,
    int num_bytes,
    bool is_tail,
    node_id toaddr,
    node_id fromaddr,
    int vn) :
   pisces_routable_packet(msg, num_bytes, is_tail, toaddr, fromaddr),
   vn_(vn)
  {
  }

  uint64_t
  flow_id() const override {
    spkt_abort_printf("simple network does not use flow IDs");
    return 0;
  }

  SST::Interfaces::SimpleNetwork::Request*
  request() const {
    return dynamic_cast<SST::Interfaces::SimpleNetwork::Request*>(orig_);
  }

  int vn() const {
    return vn_;
  }

 private:
  int vn_;

};

/**
 * @brief The simple_network_message class MTL message (flow)
 * that carries a request directly between nodes via the LogP overlay network.
 * This does not hop through PISCES switches and skips detailed congestion modeling.
 * Usually used for basic control messages.
 */
class simple_network_message : public network_message
{
 public:
  simple_network_message(SST::Interfaces::SimpleNetwork::Request* req,
                         node_id to, node_id from, int bytes) :
    network_message(to, from, bytes),
    req_(req)
  {
  }

  SST::Interfaces::SimpleNetwork::Request* req() const {
    return req_;
  }

 private:
  SST::Interfaces::SimpleNetwork::Request* req_;
};

/**
 * @brief The pisces_simple_network class  Implements the SimpleNetwork
 *  subcomponent interface for plugging into other SST elements.
 */
class pisces_simple_network :
  public SST::Interfaces::SimpleNetwork,
  public event_scheduler
{
 public:
  pisces_simple_network(sprockit::sim_parameters *params, SST::Component* comp);

  std::string to_string() const override {
    return "PISCES simple network";
  }

  /**
   * @brief init_links  Search for input, output, an in-out links to
   *        setup connections to PISCES switches
   * @param parmas
   */
  void init_links(sprockit::sim_parameters* params);

  /**
   * @brief packet_arrived Callback when first flit from packet arrives off the network
   * @param ev
   */
  void packet_head_arrived(event* ev);

  /**
   * @brief packet_tail_arrived Callback when all flits from the packet arrives off the network
   * @param pkt
   */
  void packet_tail_arrived(simple_network_packet* pkt);

  /**
   * @brief credit_arrived Callback when credit arrives from injection switch allowing
   *      more packets to be injected
   * @param ev
   */
  void credit_arrived(event* ev);

  /**
   * @brief ctrl_msg_arrived Callback invoked when simple_network_message arrives
   *  from LogP overlay network with control data (usually RDMA get request or similar)
   * @param ev
   */
  void ctrl_msg_arrived(event* ev);

  /**
   * @brief send Push a request onto the NIC to send a request to a destination node.
   * @param req
   * @param vn  The network to use. Right now, vn=0 is assumed to be a payload network for heavy traffic.
   *            vn=1 is for short control messages that should have priority and not conflict with heavy
   *            traffic. vn=1 hops onto LogP switches to avoid detailed congestion modeling.
   * @return
   */
  bool send(Request *req, int vn) override;

  /**
   * @brief recv  Any fully arrived packet (request) should be popped and returned
   * @param vn
   * @return Any requests that have arrived. nullptr if none available.
   */
  Request* recv(int vn) override;

  bool requestToReceive(int vn) override;

  bool initialize(const std::string &portName,
                 const SST::UnitAlgebra &link_bw,
                 int vns,
                 const SST::UnitAlgebra &in_buf_size,
                 const SST::UnitAlgebra &out_buf_size) override;

  void setNotifyOnSend(HandlerBase* functor) override {
   send_functor_ = functor;
  }

  void setNotifyOnReceive(HandlerBase* functor) override {
   recv_functor_ = functor;
  }

  bool isNetworkInitialized() const override {
   return initialized_;
  }

  nid_t getEndpointID() const override {
   return nid_;
  }

  const SST::UnitAlgebra& getLinkBW() const override {
   return sst_link_bw_;
  }

  bool spaceToSend(int vn, int num_bits) override;

  using SST::Interfaces::SimpleNetwork::Request;

  virtual void sendInitData(Request* req) override;

  virtual Request* recvInitData() override;

 private:
  device_id init_loc(sprockit::sim_parameters* params);

  bool send_pisces_network(Request* req, int vn);

  bool send_logp_network(Request* req, int vn);

  nid_t nid_;

  std::list<simple_network_packet*> vn0_pkts_;
  std::list<simple_network_message*> vn1_msgs_;

  pisces_injection_buffer* inj_buffer_;
  pisces_bandwidth_arbitrator* arb_;  //arbitrator for computing message delays
  SST::Link* logp_link_;   //used for sending control messages to LogP overlay network
  SST::Link* credit_link_; //used for returning credits to ejection switch
  int num_injection_credits_; //buffer space available on NIC

  SST::UnitAlgebra sst_link_bw_;
  HandlerBase* send_functor_;
  HandlerBase* recv_functor_;
  bool initialized_;

};

}
}

#endif // PISCES_SIMPLE_NETWORK_H