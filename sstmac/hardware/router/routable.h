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

#ifndef sstmac_hardware_network_interconnect_INTERCONNECT_MESSAGE_H
#define sstmac_hardware_network_interconnect_INTERCONNECT_MESSAGE_H

#include <sstmac/common/messages/sst_message.h>
#include <sstmac/hardware/router/routing_enum.h>

namespace sstmac {
namespace hw {

class routable
{
 public:
  typedef enum {
   valiant_stage,
   final_stage,
   crossed_timeline
  } metadata_slot;

  struct path {
   int outport;
   int vc;
   /** An identifier indicating what geometric path on the topology this is following */
   int geometric_id;
   sprockit::metadata_bits<uint32_t> metadata;

   path() :
     outport(routing::uninitialized),
  #if SSTMAC_SANITY_CHECK
     vc(routing::uninitialized)
  #else
     vc(0)
  #endif
   {
   }

   bool
   metadata_bit(metadata_slot slot) const {
     return metadata.bit(slot);
   }

   void
   set_metadata_bit(metadata_slot slot) {
     metadata.set_bit(slot);
   }

   void
   unset_metadata_bit(metadata_slot slot) {
     metadata.unset_bit(slot);
   }

   void
   clear_metadata() {
     metadata.clear();
   }
  };

 #define MAX_PATHS 32
 class path_set
 {
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

 public:
  node_id
  toaddr() const {
    return toaddr_;
  }

  node_id
  fromaddr() const {
    return fromaddr_;
  }

  void
  set_toaddr(node_id to) {
    toaddr_ = to;
  }

  void
  set_fromaddr(node_id from) {
    fromaddr_ = from;
  }

  path&
  current_path() {
    return path_;
  }

  void
  check_vc() {
    if (path_.vc == routing::uninitialized)
      path_.vc = 0;
  }

  virtual int
  vc() const {
    return path_.vc;
  }

  int
  port() const {
    return path_.outport;
  }

  void
  serialize_order(serializer& ser);


  void
  set_dest_switch(switch_id sid) {
    dest_switch_ = sid;
  }

  switch_id
  dest_switch() const {
    return dest_switch_;
  }

 protected:
  routable() {}

  routable(node_id toaddr, node_id fromaddr);

 private:
  node_id toaddr_;

  node_id fromaddr_;

  path path_;

  switch_id dest_switch_;

};

}
}

START_SERIALIZATION_NAMESPACE
template <>
class serialize<sstmac::hw::routable::path>
{
 public:
  void
  operator()(sstmac::hw::routable::path& info, serializer& ser){
    ser.primitive(info);
  }
};
END_SERIALIZATION_NAMESPACE

#endif // INTERCONNECT_MESSAGE_H