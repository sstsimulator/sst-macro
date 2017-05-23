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

#ifndef SPROCKIT_COMMON_MESSAGES_spkt_serializer_H_INCLUDED
#define SPROCKIT_COMMON_MESSAGES_spkt_serializer_H_INCLUDED

#include <sprockit/spkt_config.h>
#include <sprockit/serialize_packer.h>
#include <sprockit/serialize_sizer.h>
#include <sprockit/serialize_unpacker.h>
#include <typeinfo>

#include <cstring>
#include <list>
#include <vector>
#include <map>
#include <set>

namespace sprockit {

/**
  * This class is basically a wrapper for objects to declare the order in
  * which their members should be ser/des
  */
class serializer
{
 public:
  typedef enum {
    SIZER, PACK, UNPACK
  } SERIALIZE_MODE;

 public:
  serializer() :
    mode_(SIZER) //just sizing by default
  {
  }

  pvt::ser_packer&
  packer() {
    return packer_;
  }

  pvt::ser_unpacker&
  unpacker() {
    return unpacker_;
  }

  pvt::ser_sizer&
  sizer() {
    return sizer_;
  }
  
  template <class T>
  void
  size(T& t){
    sizer_.size<T>(t);
  }
  
  template <class T>
  void
  pack(T& t){
    packer_.pack<T>(t);
  }
  
  template <class T>
  void
  unpack(T& t){
    unpacker_.unpack<T>(t);
  }

  virtual
  ~serializer() {
  }

  SERIALIZE_MODE
  mode() const {
    return mode_;
  }

  void
  set_mode(SERIALIZE_MODE mode) {
    mode_ = mode;
  }

  void
  reset(){
    sizer_.reset();
    packer_.reset();
    unpacker_.reset();
  }

  template<typename T>
  void
  primitive(T &t) {
    switch(mode_)
    {
    case SIZER:
      sizer_.size(t);
      break;
    case PACK:
      packer_.pack(t);
      break;
    case UNPACK:
      unpacker_.unpack(t);
      break;
    }
  }
  
  template <class T, int N>
  void
  array(T arr[N]){
    switch (mode_)
    {
    case SIZER: {
      sizer_.add(sizeof(T) * N);
      break;
    }
    case PACK: {
      char* charstr = packer_.next_str(N*sizeof(T));
      ::memcpy(charstr, arr, N*sizeof(T));
      break;
    }
    case UNPACK: {
      char* charstr = unpacker_.next_str(N*sizeof(T));
      ::memcpy(arr, charstr, N*sizeof(T));
      break;
    }   
    }
  }

  template <typename T, typename Int>
  void
  binary(T*& buffer, Int& size){
    switch (mode_)
    {
    case SIZER: {
      sizer_.add(sizeof(Int));
      sizer_.add(size*sizeof(T));
      break;
    }
    case PACK: {
      if (buffer && size){
        pack(size);
        packer_.pack_buffer(buffer, size*sizeof(T));
      } else {
        Int sz(0);
        pack(sz);
      }
      break;
    }
    case UNPACK: {
      unpacker_.unpack(size);
      unpacker_.unpack_buffer(&buffer, size*sizeof(T));
      break;
    }
    }
  }

  template <typename Int>
  void
  binary(void*& buffer, Int& size){
    switch (mode_)
    {
    case SIZER: {
      sizer_.add(sizeof(Int));
      if (buffer) sizer_.add(size);
      break;
    }
    case PACK: {
      if (buffer && size){
        pack(size);
        packer_.pack_buffer(buffer, size);
      } else {
        Int sz(0);
        pack(sz);
      }
      break;
    }
    case UNPACK: {
      unpacker_.unpack(size);
      unpacker_.unpack_buffer(&buffer, size);
      break;
    }
    }
  }

  
  void
  string(std::string& str);

  void
  start_packing(char* buffer, size_t size){
    packer_.init(buffer, size);
    mode_ = PACK;
  }

  void
  start_sizing(){
    sizer_.reset();
    mode_ = SIZER;
  }

  void
  start_unpacking(char* buffer, size_t size){
    unpacker_.init(buffer, size);
    mode_ = UNPACK;
  }

  size_t
  size() const {
    switch (mode_){
      case SIZER: return sizer_.size();
      case PACK: return packer_.size();
      case UNPACK: return unpacker_.size();
    }
  }

 protected:
  //only one of these is going to be valid for this spkt_serializer
  //not very good class design, but a little more convenient
  pvt::ser_packer packer_;
  pvt::ser_unpacker unpacker_;
  pvt::ser_sizer sizer_;
  SERIALIZE_MODE mode_;

};

} // end of namespace sprockit
#endif