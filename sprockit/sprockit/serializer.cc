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

#include <sprockit/serializer.h>
#include <sprockit/serializable.h>
#include <sprockit/serialize.h>

RegisterDebugSlot(serialize);

namespace sprockit {
namespace pvt {

void
ser_unpacker::unpack_buffer(void* buf, int size)
{
  //void* only for convenience... actually a void**
  void** bufptr = (void**) buf;
  if (size == 0){
    *bufptr = 0;
  } else {
    *bufptr = new char[size];
    char* charstr = next_str(size);
    ::memcpy(*bufptr, charstr, size);
  }
}

void
ser_packer::pack_buffer(void* buf, int size)
{
  char* charstr = next_str(size);
  ::memcpy(charstr, buf, size);
}

void
ser_unpacker::unpack_string(std::string& str)
{
  int size;
  unpack(size);
  char* charstr = next_str(size);
  str.resize(size);
  str.assign(charstr, size);
}

void
ser_packer::pack_string(std::string& str)
{
  int size = str.size();
  pack(size);
  char* charstr = next_str(size);
  ::memcpy(charstr, str.data(), size);
}

void
ser_sizer::size_string(std::string &str)
{
  size_ += sizeof(int);
  size_ += str.size();
}

} //end ns pvt

void
serializer::string(std::string& str)
{
  switch(mode_)
  {
  case SIZER: {
    sizer_.size_string(str);
    break;
  }
  case PACK: {
    packer_.pack_string(str);
    break;
  }
  case UNPACK: {
    unpacker_.unpack_string(str);
    break;
  }
  }
}

} // end of namespace sprockit