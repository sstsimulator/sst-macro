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

#include <sprockit/ptr_type.h>
#include <sprockit/spkt_string.h>
#include <sprockit/serialize_serializable.h>

namespace sprockit {
namespace pvt {

static const long null_ptr_id = -1;

void
size_serializable(serializable* s, serializer& ser){
  long dummy = 0;
  ser.size(dummy);
  if (s) {
    s->serialize_order(ser);
  }
}

void
pack_serializable(serializable* s, serializer& ser){
  if (s) {
    debug_printf(dbg::serialize,
      "packing object with class id %ld: %s",
      s->cls_id(), s->cls_name());
    long cls_id = s->cls_id();
    ser.pack(cls_id);
    s->serialize_order(ser);
  }
  else {
    debug_printf(dbg::serialize, "null object");
    long id = null_ptr_id;
    ser.pack(id);
  }
}

void
unpack_serializable(serializable*& s, serializer& ser){
  long cls_id;
  ser.unpack(cls_id);
  if (cls_id == null_ptr_id) {
    debug_printf(dbg::serialize, "null pointer object");
    
  }
  else {
    debug_printf(dbg::serialize, "unpacking class id %ld", cls_id);
    s = sprockit::serializable_factory::get_serializable(cls_id);
    s->serialize_order(ser);
    debug_printf(dbg::serialize, "unpacked object %s", s->cls_name());
  }
}

}
}