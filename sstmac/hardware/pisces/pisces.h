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

#ifndef PACKETFLOW_H
#define PACKETFLOW_H

#include <sstmac/hardware/common/packet.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/hardware/router/routing_enum.h>
#include <sstmac/hardware/router/routable.h>
#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>

DeclareDebugSlot(pisces)
DeclareDebugSlot(pisces_queue)
DeclareDebugSlot(pisces_config)

namespace sstmac {
namespace hw {

/**
 @class pisces
 Encapsulates a group of machine packets traveling together on the
 same path between endpoints.  This is usually one fraction of
 a larger message.
 */
class pisces_payload :
  public packet
{
 public:
  static const double uninitialized_bw;

 public:
  pisces_payload(
    serializable* msg,
    int num_bytes,
    bool is_tail);

  virtual ~pisces_payload() {}

  /**
    Needed because of routable_message ambiguity.
  */
  int
  vc() const {
    return vc_;
  }

  virtual int
  next_vc() const = 0;

  virtual int
  next_port() const = 0;

  void
  update_vc() {
    int new_vc = next_vc();
    if (new_vc == routing::uninitialized){
      vc_ = 0;
    } else {
      vc_ = new_vc;
    }
  }

  /**
   @return The number of bytes in this pisces, NOT
   the total number of bytes in the parent message.
   See #num_bytes_total
   */
  int
  num_bytes() const {
    return num_bytes_;
  }

  timestamp
  arrival() const {
    return arrival_;
  }

  void
  set_arrival(timestamp time) {
    arrival_ = time;
  }

  void
  init_bw(double bw) {
    bw_ = bw_ == uninitialized_bw ? bw : bw_;
  }

  void
  set_max_bw(double bw){
    init_bw(bw);
    bw_ = std::min(bw_, bw);
  }

  /**
   @return The bandwidth in number of bytes per second
   */
  double
  bw() const {
    return bw_;
  }

  /**
   @param The bandwidth in number of bytes per second
   */
  void
  set_bw(double bw) {
    bw_ = bw;
  }

  double
  max_incoming_bw() const {
    return max_in_bw_;
  }

  void
  set_max_incoming_bw(double bw) {
    max_in_bw_ = bw;
  }

  double
  ser_delay() const {
    return num_bytes_ / bw_;
  }

  void
  set_inport(int port) {
    inport_ = port;
  }

  int
  inport() const {
    return inport_;
  }

  void
  serialize_order(serializer& ser) override;

 protected:
  pisces_payload(){} //for serialization

  int inport_;

  double bw_;

  double max_in_bw_;

  timestamp arrival_;

  int vc_;

};

/**
 */
class pisces_routable_packet :
 public pisces_payload,
 public routable
{
  public:
   pisces_routable_packet(
     serializable* msg,
     int num_bytes,
     bool is_tail,
     node_id toaddr,
     node_id fromaddr) :
    pisces_payload(msg, num_bytes, is_tail),
    routable(toaddr, fromaddr)
  {
  }

  node_id
  toaddr() const override {
   return routable::toaddr();
  }

  node_id
  fromaddr() const override {
    return routable::fromaddr();
  }

  int
  next_port() const override {
    return routable::port();
  }

  int
  next_vc() const override {
    return routable::vc();
  }

 protected:
  void
  serialize_order(serializer& ser) override;

  pisces_routable_packet(){} //serialization
};

/**
 * @brief The pisces_packet class
 * The default packet flow class using the default routable constructs
 */
class pisces_default_packet :
 public pisces_routable_packet
{
  ImplementSerializable(pisces_default_packet)
 public:
  pisces_default_packet(
   serializable* msg,
   uint64_t flow_id,
   int num_bytes,
   bool is_tail,
   node_id toaddr,
   node_id fromaddr) :
  pisces_routable_packet(msg, num_bytes, is_tail, toaddr, fromaddr),
  flow_id_(flow_id)
  {
  }

  pisces_default_packet(){} //for serialization

  uint64_t
  flow_id() const override {
    return flow_id_;
  }

  void
  serialize_order(serializer& ser) override;

  std::string
  to_string() const override;

 private:
  uint64_t flow_id_;

};

class pisces_delay_stats_packet : public pisces_default_packet
{
  ImplementSerializable(pisces_delay_stats_packet)
 public:
  pisces_delay_stats_packet(
   serializable* msg,
   uint64_t flow_id,
   int num_bytes,
   bool is_tail,
   node_id toaddr,
   node_id fromaddr) :
  pisces_default_packet(msg, flow_id, num_bytes, is_tail, toaddr, fromaddr),
   congestion_delay_(0.)
  {
  }

  pisces_delay_stats_packet(){} //for serialization

  /**
   * @brief congestion_delay
   * @return The congestion delay in seconds
   */
  double
  congestion_delay() const {
    return congestion_delay_;
  }

  void
  serialize_order(serializer& ser) override;

  void
  accumulate_delay(double sec){
   congestion_delay_ += sec;
  }

 private:
  double congestion_delay_;

};

class pisces_credit :
  public event,
  public sprockit::printable
{

 public:
  ImplementSerializable(pisces_credit)

 public:
  pisces_credit(){} //for serialization

  pisces_credit(
    int port,
    int vc,
    int num_credits)
    : port_(port),
      num_credits_(num_credits),
      vc_(vc)
  {
  }

  int
  vc() const {
    return vc_;
  }

  int
  port() const {
    return port_;
  }

#if !SSTMAC_INTEGRATED_SST_CORE
  bool is_payload() const override {
    return false;
  }

  bool is_ack() const override {
    return true;
  }
#endif

  int
  num_credits() const {
    return num_credits_;
  }

  std::string
  to_string() const override;

  void
  serialize_order(serializer& ser) override;

 protected:
  int num_credits_;
  int port_;
  int vc_;


};

}
}


#endif // PACKETFLOW_H