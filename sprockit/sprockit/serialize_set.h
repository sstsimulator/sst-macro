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

#ifndef SERIALIZE_SET_H
#define SERIALIZE_SET_H

#include <set>
#include <sprockit/unordered.h>
#include <sprockit/serializer.h>

namespace sprockit {

namespace pvt {

template <class Set, class T>
void
serialize_set(Set& v, serializer& ser) {
  typedef typename Set::iterator iterator;
  switch(ser.mode())
  {
  case serializer::SIZER: {
    size_t size = v.size();
    ser.size(size);
    iterator it, end = v.end();
    for (it=v.begin(); it != end; ++it){
      T& t = const_cast<T&>(*it); 
      serialize<T>()(t,ser);
    }
    break;
  }
  case serializer::PACK: {
    size_t size = v.size();
    ser.pack(size);
    iterator it, end = v.end();
    for (it=v.begin(); it != end; ++it){
      T& t = const_cast<T&>(*it); 
      serialize<T>()(t,ser);
    }
    break;
  }
  case serializer::UNPACK: {
    size_t size;
    ser.unpack(size);
    for (int i=0; i < size; ++i){
      T t;
      serialize<T>()(t,ser);
      v.insert(t);
    }
    break;
  }
  }
}

} //end ns pvt

template <class T>
class serialize<std::set<T> > {
  typedef std::set<T> Set;
 public:
  void
  operator()(Set& v, serializer& ser) {
    pvt::serialize_set<Set,T>(v,ser);
  }
};

#if !SPKT_ENABLE_ORDERED_MAP
template <class T>
class serialize<spkt_unordered_set<T> > {
  typedef spkt_unordered_set<T> Set;
 public:
  void
  operator()(Set& v, serializer& ser) {
    pvt::serialize_set<Set,T>(v,ser);
  }
};
#endif

}

#endif // SERIALIZE_SET_H