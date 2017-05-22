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

#ifndef SERIALIZE_ACCESSOR_H
#define SERIALIZE_ACCESSOR_H

#include <cstring>
#include <sprockit/errors.h>

namespace sprockit {

class ser_buffer_overrun : public spkt_error {
 public:
  ser_buffer_overrun(int maxsize) :
    spkt_error(sprockit::printf("serialization overrun buffer of size %d", maxsize))
  {
  }
};

namespace pvt {


class ser_buffer_accessor {
 public:
  template <class T>
  T*
  next(){
    T* ser_buffer = reinterpret_cast<T*>(bufptr_);
    bufptr_ += sizeof(T);
    size_ += sizeof(T);
    if (size_ > max_size_) throw ser_buffer_overrun(max_size_);
    return ser_buffer;
  }

  char*
  next_str(size_t size){
    char* ser_buffer = reinterpret_cast<char*>(bufptr_);
    bufptr_ += size;
    size_ += size;
    if (size_ > max_size_) throw ser_buffer_overrun(max_size_);
    return ser_buffer;
  }

  size_t
  size() const {
    return size_;
  }

  size_t
  max_size() const {
    return max_size_;
  }

  void
  init(void* buffer, size_t size){
    bufstart_ = reinterpret_cast<char*>(buffer);
    max_size_ = size;
    reset();
  }

  void
  clear(){
    bufstart_ = bufptr_ = 0;
    max_size_ = size_ = 0;
  }

  void
  reset(){
    bufptr_ = bufstart_;
    size_ = 0;
  }

 protected:
  ser_buffer_accessor() :
    bufstart_(nullptr),
    bufptr_(nullptr),
    size_(0)
  {
  }

 protected:
  char* bufstart_;
  char* bufptr_;
  size_t size_;
  size_t max_size_;

};

} }

#endif // SERIALIZE_ACCESSOR_H