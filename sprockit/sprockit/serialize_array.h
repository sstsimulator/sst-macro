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

#ifndef SERIALIZE_ARRAY_H
#define SERIALIZE_ARRAY_H

#include <sprockit/serializer.h>

namespace sprockit {
namespace pvt {

template <class TPtr, class IntType>
class ser_array_wrapper
{
 public:
  TPtr& bufptr;
  IntType& sizeptr;
  ser_array_wrapper(TPtr& buf, IntType& size) :
    bufptr(buf), sizeptr(size) {}

};

template <class TPtr>
class raw_ptr_wrapper
{
public:
    TPtr*& bufptr;
    raw_ptr_wrapper(TPtr*& ptr) :
        bufptr(ptr) {}
};

}

template <class T, int N>
class serialize<T[N]> {
 public:
  void operator()(T arr[N], serializer& ser){
    ser.array<T,N>(arr);
  }
};

/** I have typedefing pointers, but no other way.
 *  T could be "void and TPtr void* */
template <class TPtr, class IntType>
pvt::ser_array_wrapper<TPtr,IntType>
array(TPtr& buf, IntType& size)
{
  return pvt::ser_array_wrapper<TPtr,IntType>(buf, size);
}

template <class TPtr>
inline pvt::raw_ptr_wrapper<TPtr>
raw_ptr(TPtr*& ptr)
{
  return pvt::raw_ptr_wrapper<TPtr>(ptr);
}

template <class TPtr, class IntType>
inline void
operator&(serializer& ser, pvt::ser_array_wrapper<TPtr,IntType> arr){
  ser.binary(arr.bufptr, arr.sizeptr);
}

// Needed only because the default version in serialize.h can't get
// the template expansions quite right trying to look through several
// levels of expansion
template <class TPtr>
inline void
operator&(serializer& ser, pvt::raw_ptr_wrapper<TPtr> ptr){
  ser.primitive(ptr.bufptr);
}

}

#endif // SERIALIZE_ARRAY_H