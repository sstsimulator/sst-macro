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

#ifndef SERIALIZE_H
#define SERIALIZE_H

#include <sprockit/serializer.h>
#include <sprockit/serializable_type.h>
#include <sprockit/debug.h>
#include <type_traits>

DeclareDebugSlot(serialize);

namespace sprockit {

template <class T>
class serialize {
 public:
  inline void operator()(T& t, serializer& ser){
    ser.primitive(t); 
  }
};

template <>
class serialize<bool> {
 public:
  void operator()(bool &t, serializer& ser){
    int bval = t;
    ser.primitive(bval);
    t = bool(bval);
  }
};

namespace pvt {

void
size_serializable(serializable* s, serializer& ser);

void
pack_serializable(serializable* s, serializer& ser);

void
unpack_serializable(serializable*& s, serializer& ser);

}


template <>
class serialize<serializable*> {

 public:
  void
  operator()(serializable*& s, serializer& ser)
  {
    switch (ser.mode()){
  case serializer::SIZER:
    pvt::size_serializable(s,ser);
    break;
  case serializer::PACK:
    pvt::pack_serializable(s,ser);
    break;
  case serializer::UNPACK:
    pvt::unpack_serializable(s,ser);
    break;
    }
  }

};

template <class T,bool flag>
class serialize_ptr
{
 public:
  void
  operator()(serializer& ser, T*& t){
    abort();
  }
};

template <class T>
class serialize_ptr<T,false>
{
 public:
  void
  operator()(serializer& ser, T*& t){
    ser.primitive(t);
  }
};

template <class T>
class serialize_ptr<T,true>
{
 public:
  void
  operator()(serializer& ser, T*& t){
    serializable* s = t;
    serialize<serializable*>()(s, ser);
    t = static_cast<T*>(s);
  }
};

inline void
operator&(serializer& ser, void* v){
  ser.primitive(v);
}

template <class T>
inline void
operator&(serializer& ser, T*& t){
  serialize_ptr<T,std::is_base_of<serializable,T>::value>()(ser, t);
}

template <class T>
inline void
operator&(serializer& ser, T& t){
  serialize<T>()(t, ser);
}

}

#include <sprockit/serialize_array.h>
#include <sprockit/serialize_list.h>
#include <sprockit/serialize_map.h>
#include <sprockit/serialize_set.h>
#include <sprockit/serialize_vector.h>
#include <sprockit/serialize_string.h>

#endif // SERIALIZE_H