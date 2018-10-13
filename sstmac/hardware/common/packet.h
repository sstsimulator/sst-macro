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

#ifndef sstmac_hardware_packet_h
#define sstmac_hardware_packet_h

#include <sstmac/common/sst_event.h>
#include <sstmac/common/messages/sst_message_fwd.h>
#include <sstmac/hardware/router/routing_enum.h>

namespace sstmac {
namespace hw {

#define MAX_HEADER_BYTES 16
#define MAX_CONTROL_BYTES 8
#define MAX_NIC_BYTES 8
#define MAX_STAT_BYTES 8
class packet :
  public event,
  public sprockit::printable
{

 public:
  struct header {
    char is_tail : 1;
    uint16_t port;
    uint8_t row : 4;
    uint8_t col : 4;
    uint8_t vc : 4;
  };

  serializable* orig() const {
    return orig_;
  }

  virtual std::string to_string() const override {
    return "packet";
  }

  template <class T>
  T* rtr_header() {
    static_assert(sizeof(T) <= sizeof(rtr_metadata_),
                  "given header type too big");
    return (T*) (&rtr_metadata_);
  }

  template <class T>
  const T* rtr_header() const {
    static_assert(sizeof(T) <= sizeof(rtr_metadata_),
                  "given header type too big");
    return (T*) (&rtr_metadata_);
  }

  template <class T>
  T* control_header() {
    static_assert(sizeof(T) <= sizeof(control_flow_metadata_),
                  "given header type too big");
    return (T*) (&control_flow_metadata_);
  }

  template <class T>
  const T* control_header() const {
    static_assert(sizeof(T) <= sizeof(control_flow_metadata_),
                  "given header type too big");
    return (T*) (&control_flow_metadata_);
  }

  node_id toaddr() const {
    return toaddr_;
  }

  node_id fromaddr() const {
    return fromaddr_;
  }

  void set_toaddr(node_id to) {
    toaddr_ = to;
  }

  void set_fromaddr(node_id from) {
    fromaddr_ = from;
  }

  int vc() const {
    auto hdr = rtr_header<header>();
    return hdr->vc;
  }

  void set_outport(const int port) {
    auto hdr = rtr_header<header>();
    hdr->port = port;
  }

  void set_vc(const int vc) {
    auto hdr = rtr_header<header>();
    hdr->vc = vc;
  }

  int outport() const {
    auto hdr = rtr_header<header>();
    return hdr->port;
  }

  virtual void serialize_order(serializer& ser) override;

  bool is_tail() const {
    auto hdr = rtr_header<header>();
    return hdr->is_tail;
  }

  uint32_t byte_length() const {
    return num_bytes_;
  }

  uint32_t num_bytes() const {
    return num_bytes_;
  }

  uint64_t flow_id() const {
    return flow_id_;
  }

 private:
  node_id toaddr_;

  node_id fromaddr_;

  uint64_t flow_id_;

  serializable* orig_;

  char rtr_metadata_[MAX_HEADER_BYTES];

  char control_flow_metadata_[MAX_CONTROL_BYTES];

  char nic_metadata_[MAX_NIC_BYTES];

  char stats_metadata_[MAX_STAT_BYTES];

  uint32_t num_bytes_;

 protected:
  packet() : packet(nullptr, 0, 0, false, 0, 0) {}

  packet(serializable* payload,
    uint32_t num_bytes,
    uint64_t flow_id,
    bool is_tail,
    node_id fromaddr,
    node_id toadadr);

};

}
}

#endif
