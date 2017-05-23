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

#ifndef EVENT_LOCATION_H
#define EVENT_LOCATION_H

#include <sstmac/common/node_address.h>
#include <sprockit/errors.h>
#include <sstmac/common/serializable.h>

namespace sstmac {

struct device_id {
  typedef enum {
    node=0,
    router=1,
    netlink=2,
    logp_overlay=3,
    control_event=4,
    null=5,
  } type_t;

  explicit device_id(uint32_t id, type_t ty) :
    location_(id), type_(ty)
  {
  }

  static device_id
  ctrl_event() {
    return device_id(0,control_event);
  }

  device_id() :
    location_(0), type_(null)
  {
  }

  type_t
  type() const {
    return type_;
  }

  uint32_t
  id() const {
    return location_;
  }

  bool
  is_node_id() const {
    return type_ == node;
  }

  bool
  is_switch_id() const {
    return type_ == router;
  }

  bool
  is_netlink_id() const {
    return type_ == netlink;
  }

 private:
  uint32_t location_;
  type_t type_;

};

inline bool
operator==(const device_id& a, const device_id& b){
  return a.id() == b.id() && a.type() == b.type();
}

inline bool
operator<(const device_id& a, const device_id& b){
  if (a.id() != b.id()) return a.id() < b.id();
  else return a.type() < b.type();
}

}

START_SERIALIZATION_NAMESPACE
template <>
class serialize<sstmac::device_id>
{
 public:
  void
  operator()(sstmac::device_id& t, serializer& ser){
    ser.primitive(t);
  }
};
END_SERIALIZATION_NAMESPACE


#endif // EVENT_LOCATION_H