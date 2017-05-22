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

#ifndef SERIALIZE_MAP_H
#define SERIALIZE_MAP_H

#include <map>
#include <sprockit/unordered.h>
#include <sprockit/serializer.h>

namespace sprockit {

namespace pvt {

template <class Map, class Key, class Value>
void
serialize_map(Map& m, serializer& ser)
{
  typedef typename Map::iterator iterator;
  switch(ser.mode())
  {
  case serializer::SIZER: {
    size_t size = m.size();
    ser.size(size);
    iterator it, end = m.end();
    for (it=m.begin(); it != end; ++it){
      //keys are const values - annoyingly
      serialize<Key>()(const_cast<Key&>(it->first), ser);
      serialize<Value>()(it->second, ser);
    }
    break;
  }
  case serializer::PACK: {
    size_t size = m.size();
    ser.pack(size);
    iterator it, end = m.end();
    for (it=m.begin(); it != end; ++it){
      serialize<Key>()(const_cast<Key&>(it->first), ser);
      serialize<Value>()(it->second, ser);
    }
    break;
  }
  case serializer::UNPACK: {
    size_t size;
    ser.unpack(size);
    for (int i=0; i < size; ++i){
      Key k;
      Value v;
      serialize<Key>()(k, ser);
      serialize<Value>()(v, ser);
      m[k] = v;
    }
    break;
  }
  }    
}

} //end ns private

template <class Key, class Value>
class serialize<std::map<Key,Value> > {
  typedef std::map<Key,Value> Map;
 public:
  void operator()(Map& m, serializer& ser){
    pvt::serialize_map<Map,Key,Value>(m,ser);
  }
};

#if !SPKT_ENABLE_ORDERED_MAP
template <class Key, class Value>
class serialize<spkt_unordered_map<Key,Value> > {
  typedef spkt_unordered_map<Key,Value> Map;
 public:
  void operator()(Map& m, serializer& ser){
    pvt::serialize_map<Map,Key,Value>(m,ser);
  }
};
#endif

}

#endif // SERIALIZE_MAP_H