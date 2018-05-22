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
#define MAX_STAT_BYTES 8
class packet :
  public event,
  public sprockit::printable
{

 public:
  struct header {
    char is_tail : 1;
    uint32_t dest_switch : 22;
  };

  serializable* orig() const {
    return orig_;
  }

  virtual std::string to_string() const override {
    return "packet";
  }

  template <class T>
  T* get_header() {
    static_assert(sizeof(T) <= sizeof(header_metadata_),
                  "given header type too big");
    return (T*) (&header_metadata_);
  }

  template <class T>
  const T* get_header() const {
    static_assert(sizeof(T) <= sizeof(header_metadata_),
                  "given header type too big");
    return (T*) (&header_metadata_);
  }


  struct path {
   struct outport_t {
     uint16_t global;
     uint16_t local;
   };
   outport_t outport_;
   int vc;
   /** An identifier indicating what geometric path on the topology this is following */
   int geometric_id;

   path() :
  #if SSTMAC_SANITY_CHECK
     vc(routing::uninitialized)
  #else
     vc(0)
  #endif
   {
     outport_.global = routing::uninitialized;
     outport_.local = routing::uninitialized;
   }

   uint16_t& outport() {
     return outport_.global;
   }

   uint16_t& global_outport() {
     return outport_.global;
   }

   uint16_t& local_outport() {
     return outport_.local;
   }

   void set_outport(const uint16_t port) {
     outport_.global = port;
   }

   void set_global_outport(const uint16_t port) {
     outport_.global = port;
   }

   void set_local_outport(const uint16_t port) {
     outport_.local = port;
   }
  };

  #define MAX_PATHS 32
  class path_set {
   public:
    path_set() : size_(0) {}
    int size() const { return size_; }
    void resize(int s){
      if (s > MAX_PATHS){
       spkt_throw_printf(sprockit::value_error,
         "routable::path_set size exceeds max %d", MAX_PATHS);
      }
      size_ = s;
    }

    path& operator[](int idx){
      return paths_[idx];
    }

   private:
    int size_;
    path paths_[MAX_PATHS];
  };

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

  path& current_path() {
    return path_;
  }

  void check_vc() {
    if (path_.vc == routing::uninitialized)
      path_.vc = 0;
  }

  int vc() const {
    return path_.vc;
  }

  void set_outport(const int port) {
    path_.outport_.global = port;
  }

  void set_global_outport(const int port) {
    path_.outport_.global = port;
  }

  void set_local_outport(const int port) {
    path_.outport_.local = port;
  }

  int outport() const {
    return path_.outport_.global;
  }

  int global_outport() const {
    return path_.outport_.global;
  }

  int local_outport() const {
    return path_.outport_.local;
  }

  virtual void serialize_order(serializer& ser) override;

  void set_dest_switch(switch_id sid) {
    auto hdr = get_header<header>();
    hdr->dest_switch = sid;
  }

  switch_id dest_switch() const {
    auto hdr = get_header<header>();
    return hdr->dest_switch;
  }

  bool is_tail() const {
    auto hdr = get_header<header>();
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

  path path_;

  uint64_t flow_id_;

  serializable* orig_;

  char header_metadata_[MAX_HEADER_BYTES];

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

START_SERIALIZATION_NAMESPACE
template <>
class serialize<sstmac::hw::packet::path>
{
 public:
  void operator()(sstmac::hw::packet::path& info, serializer& ser){
    ser.primitive(info);
  }
};
END_SERIALIZATION_NAMESPACE


#endif
